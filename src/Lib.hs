{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Lib where

import           System.FilePath                ( FilePath )
import           Data.UUID                      ( UUID )
import           Data.Text                      ( Text
                                                , append
                                                )
import           Data.Foldable                  ( traverse_ )
import           Control.Monad.Except           ( MonadError(..) )

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data NotionSearchRes = NotionSearchRes { imageURL :: Text, insertId :: UUID }

parsingErrorText :: Text -> Text
parsingErrorText =
  append
    "Unfortunately if failed to parse the text in the Image, if you want to try again, replace this text with add_ocr again. Error was: "

class Notion m where
  searchNotion :: m [NotionSearchRes]
  insertOCR :: Text -> UUID -> m ()

class FS m where
  downloadFile :: Text -> m FilePath
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
      imagePath <- downloadFile $ imageURL searchRes
      ocrText   <- catchError (getOCR imagePath) (pure . parsingErrorText)
      deleteFile imagePath
      insertOCR ocrText $ insertId searchRes
    )
    logErr
  )
 where
  getOCR image =
    ocrFile image >>= (\fileP -> getFile fileP `finally` deleteFile fileP)

finally :: (MonadError e m) => m a -> m b -> m a
finally comp cleanUp =
  catchError comp (\err -> cleanUp >> throwError err) <* cleanUp
