{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Updater where

import           Data.Text                      ( Text
                                                , append
                                                )
import           Data.Foldable                  ( traverse_ )
import           Control.Monad.Except           ( MonadError(..) )
import           Console
import           FS
import           Notion
import           Tesseract


parsingErrorText :: Text -> Text
parsingErrorText =
  append
    "Unfortunately if failed to parse the text in the Image, if you want to try again, replace this text with add_ocr again. Error was: "

updateOcrs
  :: (Notion m, Monad m, FS m, Tesseract m, MonadError Text m, Console m)
  => m ()
updateOcrs = searchNotion >>= traverse_
  (\searchRes -> catchError
    (do
      imagePath <- downloadFile $ imageURL searchRes
      ocrText   <- catchError (getOCR imagePath) (pure . parsingErrorText)
      deleteFile imagePath
      insertOCR ocrText $ insertId searchRes
    )
    writeOut
  )
 where
  getOCR image =
    ocrFile image >>= (\fileP -> getFile fileP `finally` deleteFile fileP)

finally :: (MonadError e m) => m a -> m b -> m a
finally comp cleanUp =
  catchError comp (\err -> cleanUp >> throwError err) <* cleanUp
