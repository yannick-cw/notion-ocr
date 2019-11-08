{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Lib
  ( someFunc
  )
where

import           System.FilePath                ( FilePath )
import           Data.UUID                      ( UUID )
import           Network.URL                    ( URL )
import           Data.Text                      ( Text
                                                , append
                                                )
import           Data.Foldable                  ( traverse_ )
import           Control.Monad.Except           ( MonadError(..) )
import           Control.Exception              ( try )

someFunc :: IO ()
someFunc = putStrLn "someFunc"


data NotionSearchRes = NotionSearchRes { imageURL :: URL, insertId :: UUID }

parsingErrorText :: Text -> Text
parsingErrorText =
  append
    "Unfortunately if failed to parse the text in the Image, if you want to try again, replace this text with add_ocr again. Error was: "

class Notion m where
  searchNotion :: m [ NotionSearchRes ]
  insertOCR :: Text -> UUID -> m ()

class FS m where
  saveFile :: URL -> m FilePath
  deleteFile :: FilePath -> m ()
  getFile :: FilePath -> m Text

class Tesseract m where
  ocrFile :: FilePath -> m FilePath

class ErrorLog m where
  logErr :: Text -> m ()

updateOcrs
  :: (Notion m, Monad m, FS m, Tesseract m, MonadError Text m, ErrorLog m)
  => m ()
updateOcrs = searchNotion >>= traverse_
  (\searchRes -> catchError
    (do
      imagePath <- saveFile $ imageURL searchRes
      ocrText   <- catchError (getOCR imagePath) (pure . parsingErrorText)
      deleteFile imagePath
      insertOCR ocrText $ insertId searchRes
    )
    logErr
  )
 where
  getOCR image =
    ocrFile image >>= (\fileP -> getFile fileP <* deleteFile fileP)
