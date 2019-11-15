{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Updater where

import           Data.Text                      ( Text
                                                , append
                                                , pack
                                                , unpack
                                                )
import           Data.Foldable                  ( traverse_ )
import           Control.Monad.Except           ( MonadError(..) )
import           Network.URI.Encode             ( encode )
import           Console
import           FS
import           Notion
import           Tesseract


parsingErrorText :: Text -> Text
parsingErrorText =
  append
    "Unfortunately if failed to parse the text in the Image, if you want to try again, replace this text with the initial command again. Error was: "

updateOcrs
  :: (Notion m, Monad m, FS m, Tesseract m, MonadError Text m, Console m)
  => m ()
updateOcrs = searchNotion >>= traverse_
  (\searchRes -> catchError
    (do
      imagePath <- downloadFile $ parseImgUrl $ imageURL searchRes
      ocrText   <- catchError (getOCR imagePath) (pure . parsingErrorText)
      deleteFile imagePath
      insertOCR ocrText $ insertId searchRes
    )
    writeOut
  )
 where
  getOCR image =
    ocrFile image >>= (\fileP -> getFile fileP `finally` deleteFile fileP)
  parseImgUrl imgUrl =
    "https://www.notion.so/image/" `append` pack (encode $ unpack imgUrl)

finally :: (MonadError e m) => m a -> m b -> m a
finally comp cleanUp =
  catchError comp (\err -> cleanUp >> throwError err) <* cleanUp
