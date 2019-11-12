{-# LANGUAGE OverloadedStrings #-}

module Lib where

import           AppM
import           Updater
import           Control.Monad.Except           ( runExceptT )
import           Control.Monad.Reader           ( runReaderT )
import           Data.Text                      ( unpack )
import           CliParser

someFunc :: IO ()
someFunc = do
  args <- parseArgs
  res  <- runReaderT (runExceptT $ unwrap runUpdate) args
  case res of
    Right _   -> return ()
    Left  err -> putStr (unpack err)
 where
  runUpdate :: AppM ()
  runUpdate = updateOcrs
