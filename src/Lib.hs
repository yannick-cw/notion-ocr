{-# LANGUAGE OverloadedStrings #-}

module Lib where

import           AppM
import           Updater
import           Control.Monad.Except           ( runExceptT )
import           Control.Monad.Reader           ( runReaderT )
import           Data.Text                      ( unpack )

someFunc :: IO ()
someFunc = do
  res <- runReaderT (runExceptT $ unwrap runUpdate) (Args "" "")
  case res of
    Right _   -> return ()
    Left  err -> putStr (unpack err)
 where
  runUpdate :: AppM ()
  runUpdate = updateOcrs
