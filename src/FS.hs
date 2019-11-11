{-# LANGUAGE OverloadedStrings #-}

module FS where

import           System.FilePath                ( FilePath )
import           Data.Text                      ( Text )
import           AppM

class FS m where
  downloadFile :: Text -> m FilePath
  deleteFile :: FilePath -> m ()
  getFile :: FilePath -> m Text

instance FS AppM where
  downloadFile _ = pure ""
  deleteFile _ = pure ()
  getFile _ = pure ""


