{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module UpdaterSpec where

import           Test.Hspec
import           Updater
import           Control.Monad.Except
import           Control.Monad.State
import           Control.Monad.Writer
import           Control.Monad.Reader           ( MonadReader )
import           Data.Functor
import           Data.Text                      ( Text
                                                , append
                                                , unpack
                                                , pack
                                                )
import           Data.UUID                      ( UUID
                                                , fromString
                                                )
import           Data.Maybe                     ( fromMaybe )
import           Control.Monad.RWS
import           Console
import           FS
import           Notion
import           Tesseract

data TestData = TestData { tesseracFail :: Bool , firstDownloadFail :: Bool , getFileFail :: Bool}

newtype TestApp a = TestApp { inner ::   ExceptT Text (RWS TestData [TestCommand] Int) a }
  deriving (Functor, Applicative, Monad, MonadWriter [TestCommand]
          , MonadError Text, MonadReader TestData, MonadState Int)

data TestCommand = NotionSearch
                 | NotionInsert Text UUID
                 | FS Text
                 | Tesseract Text
                 | ErrLog Text
  deriving (Show, Eq)

testId1 :: UUID
testId1 =
  fromMaybe undefined (fromString "1a648773-6394-4128-8059-f14a8936628b")
testId2 :: UUID
testId2 =
  fromMaybe undefined (fromString "e8f2d3cc-d8b5-4ba0-a3b5-41caee8b9419")

instance Notion TestApp where
  searchNotion =
    tell [NotionSearch]
      $> [ NotionSearchRes { imageURL = "img1", insertId = testId1 }
         , NotionSearchRes { imageURL = "img2", insertId = testId2 }
         ]
  insertOCR text inId = tell [NotionInsert text inId]

instance FS TestApp where
  downloadFile url = do
    fails <- get
    modify (+ 1)
    shouldFail <- asks firstDownloadFail
    if shouldFail && fails == 0
      then throwError "FAIL"
      else tell [FS $ "loaded file " `append` url]
        >> return ("file path: " ++ unpack url)

  deleteFile path = tell [FS $ "deleting file " `append` pack path]
  getFile path = do
    tell [FS $ "reading file " `append` pack path]
    shouldFail <- asks getFileFail
    if shouldFail
      then throwError "FAIL"
      else return ("File content of file for " `append` pack path)

instance Tesseract TestApp where
  ocrFile path = do
    tell [Tesseract $ "In file " `append` pack path]
    shouldFail <- asks tesseracFail
    if shouldFail then throwError "FAIL" else return ("OCR file for " ++ path)

instance Console TestApp where
  writeOut msg = tell [ErrLog msg]

spec :: Spec
spec = describe "updateOcrs" $ do
  it "executes the expected commands in order"
    $          basicRun
    `shouldBe` expectedCommands
  it "writes a failure message into notion, when ocr fails"
    $          (   ocrFail
               >>= (\case
                     NotionInsert content _ -> [content]
                     _                      -> []
                   )
               )
    `shouldBe` [parsingErrorText "FAIL", parsingErrorText "FAIL"]
  it "deletes the img file, even when ocr fails"
    $               ocrFail
    `shouldContain` [FS "deleting file file path: img1"]
  it "deletes the ocr file, reading of ocr file fails"
    $               getFailFails
    `shouldContain` [FS "deleting file OCR file for file path: img1"]
  it "processes the second file, when the first fails and logs the failure"
    $          firstFail
    `shouldBe` NotionSearch
    :          ErrLog "FAIL"
    :          img2Commands
 where
  tData = TestData { tesseracFail      = False
                   , firstDownloadFail = False
                   , getFileFail       = False
                   }
  basicRun     = writtenCommands tData
  ocrFail      = writtenCommands $ tData { tesseracFail = True }
  firstFail    = writtenCommands $ tData { firstDownloadFail = True }
  getFailFails = writtenCommands $ tData { getFileFail = True }
  writtenCommands :: TestData -> [TestCommand]
  writtenCommands testData =
    snd $ evalRWS (runExceptT $ inner updateOcrs) testData 0

img1Commands :: [TestCommand]
img1Commands =
  [ FS "loaded file img1"
  , Tesseract "In file file path: img1"
  , FS "reading file OCR file for file path: img1"
  , FS "deleting file OCR file for file path: img1"
  , FS "deleting file file path: img1"
  , NotionInsert "File content of file for OCR file for file path: img1" testId1
  ]

img2Commands :: [TestCommand]
img2Commands =
  [ FS "loaded file img2"
  , Tesseract "In file file path: img2"
  , FS "reading file OCR file for file path: img2"
  , FS "deleting file OCR file for file path: img2"
  , FS "deleting file file path: img2"
  , NotionInsert "File content of file for OCR file for file path: img2" testId2
  ]
expectedCommands :: [TestCommand]
expectedCommands = NotionSearch : img1Commands ++ img2Commands
