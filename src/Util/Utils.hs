{-# LANGUAGE OverloadedStrings #-}
module Util.Utils
  ( cookieOpts
  , addCookie
  )
where


import           Data.Text                     as T
                                                ( unpack )
import           CliParser
import           Control.Monad.Reader
import           Control.Monad.IO.Class         ( liftIO )
import           Data.ByteString.Char8         as C8
                                                ( pack )
import           Network.HTTP.Client            ( Cookie(..)
                                                , createCookieJar
                                                , CookieJar
                                                , Request
                                                , insertCookiesIntoRequest
                                                )
import           Data.Time.Clock
import           Control.Lens
import           Network.Wreq

notionCookie :: HasNotion r => ReaderT r IO CookieJar
notionCookie = do
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
  return $ createCookieJar [c]

addCookie :: HasNotion r => Request -> ReaderT r IO Request
addCookie req = do
  now       <- liftIO getCurrentTime
  cookieJar <- notionCookie
  return $ fst $ insertCookiesIntoRequest req cookieJar now

cookieOpts :: HasNotion r => ReaderT r IO Options
cookieOpts = (\jar -> defaults & cookies ?~ jar) <$> notionCookie
