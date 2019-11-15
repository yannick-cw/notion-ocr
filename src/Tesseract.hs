{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Tesseract
  ( Tesseract(..)
  )
where

import           AppM
import           CliParser
import           Control.Monad.Reader
import           Control.Monad.Except
import           Control.Exception              ( try )
import           Data.Text                      ( Text
                                                , pack
                                                )
import           System.Process                 ( rawSystem )
import           System.FilePath.Posix          ( (</>) )
import           System.Exit                    ( ExitCode(..) )

class Tesseract m where
    ocrFile :: FilePath -> m FilePath

instance Tesseract AppM where
  ocrFile = AppM . scanFile

scanFile :: HasTempDir r => FilePath -> ExceptT Text (ReaderT r IO) FilePath
scanFile imagePath = do
  tmpDirPath <- asks path
  let outFilePath = tmpDirPath </> "out"
  sysCode <- liftIO $ try $ rawSystem "tesseract" [imagePath, outFilePath]
  either
    liftIOErr
    (\case
      ExitSuccess -> return $ outFilePath ++ ".txt"
      (ExitFailure _) ->
        throwError $ pack $ "OCR for scanning file " ++ imagePath ++ " failed."
    )
    sysCode
