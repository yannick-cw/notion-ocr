{-# LANGUAGE OverloadedStrings #-}

module Tesseract
  ( Tesseract(..)
  )
where

import           AppM
import           Control.Monad.Reader
import           Control.Monad.Except
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
  sysCode <- liftIO $ rawSystem "tesseract" [imagePath, outFilePath]
  case sysCode of
    ExitSuccess -> return outFilePath
    (ExitFailure _) ->
      throwError $ pack $ "OCR for scanning file " ++ imagePath ++ " failed."
