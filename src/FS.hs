{-# LANGUAGE OverloadedStrings #-}

module FS where

import           System.FilePath                ( FilePath )
import           System.Directory               ( removeFile )
import           Data.Text                      ( Text
                                                , pack
                                                , unpack
                                                )
import           AppM
import           Control.Exception              ( try )
import           Control.Monad.Reader
import           Control.Monad.IO.Class         ( liftIO )
import           Network.HTTP.Simple            ( httpSource
                                                , getResponseBody
                                                , parseRequest
                                                )
import           CliParser
import           Conduit
import           System.FilePath.Posix          ( (</>) )

class FS m where
  downloadFile :: Text -> m FilePath
  deleteFile :: FilePath -> m ()
  getFile :: FilePath -> m Text

instance FS AppM where
  downloadFile url = do
    tempDir     <- asks tempPath
    req         <- liftIO $ parseRequest (unpack url)
    doneWriting <-
      liftIO $ try $ runConduitRes $ httpSource req getResponseBody .| sinkFile
        (tempDir </> "img")
    either liftIOErr (\_ -> return "") doneWriting

  deleteFile filePath = do
    deleted <- liftIO $ try $ removeFile filePath
    either liftIOErr return deleted
  getFile filePath = do
    file <- liftIO $ try $ readFile filePath
    either liftIOErr (return . pack) file



