{-# LANGUAGE OverloadedStrings #-}

module FS where

import           System.FilePath                ( FilePath )
import           System.Directory               ( removeFile )
import           Data.Text                     as T
                                                ( pack
                                                , unpack
                                                , Text
                                                )
import           AppM
import           CliParser
import           Control.Exception              ( try )
import           Control.Monad.Reader
import           Control.Monad.IO.Class         ( liftIO )
import           Network.HTTP.Simple            ( httpSource
                                                , getResponseBody
                                                , parseRequest
                                                )
import           Data.ByteString.Char8         as C8
                                                ( pack )
import           Network.HTTP.Client            ( Cookie(..)
                                                , createCookieJar
                                                , Request
                                                , insertCookiesIntoRequest
                                                )
import           Data.Time.Clock
import           Conduit
import           System.FilePath.Posix          ( (</>) )

class FS m where
  downloadFile :: Text -> m FilePath
  deleteFile :: FilePath -> m ()
  getFile :: FilePath -> m Text


instance FS AppM where
  downloadFile url = do
    tempDir       <- asks tempPath
    req           <- liftIO $ parseRequest (T.unpack url)
    reqWithCookie <- AppM $ lift $ addCookie req
    let outImg = tempDir </> "ocr_img"
    doneWriting <-
      liftIO
      $  try
      $  runConduitRes
      $  httpSource reqWithCookie getResponseBody
      .| sinkFile outImg
    either liftIOErr (const $ return outImg) doneWriting

  deleteFile filePath = do
    deleted <- liftIO $ try $ removeFile filePath
    either liftIOErr return deleted
  getFile filePath = do
    file <- liftIO $ try $ readFile filePath
    either liftIOErr (return . T.pack) file


addCookie :: HasNotion r => Request -> ReaderT r IO Request
addCookie req = do
  token <- asks notionConf
  now   <- liftIO getCurrentTime
  let expires = addUTCTime (1440 * 3000) now
      c       = Cookie "token_v2"
                       (C8.pack $ T.unpack token)
                       expires
                       "notion.so"
                       "/"
                       now
                       now
                       False
                       False
                       True
                       True
  return $ fst $ insertCookiesIntoRequest req (createCookieJar [c]) now
