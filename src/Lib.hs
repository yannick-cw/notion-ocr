{-# LANGUAGE OverloadedStrings #-}

module Lib where

import           AppM
import           Updater
import           Control.Monad.Except           ( runExceptT )
import           Control.Monad.Reader           ( runReaderT )
import           Data.Text                      ( unpack )
import           CliParser
import           Control.Concurrent             ( ThreadId
                                                , threadDelay
                                                )
import           Control.Monad                  ( void )
import           System.IO.Temp                 ( withSystemTempDirectory )


runMain :: IO ()
runMain = withSystemTempDirectory "notion_ocr" $ \tmpDir -> do
  args <- parseArgs tmpDir
  maybe (runUpdate args) (void . runScheduled (runUpdate args)) (schedule args)
 where
  runUpdate arguments = do
    res <- runReaderT (runExceptT $ unwrap updateOcrs) arguments
    case res of
      Right _   -> return ()
      Left  err -> putStr (unpack err)

runScheduled :: IO () -> Int -> IO ThreadId
runScheduled job pauseTimeMinutes = do
  putStrLn
    $  "Scheduled update to run in "
    ++ show pauseTimeMinutes
    ++ " minutes"
  threadDelay (pauseTimeMinutes * 60 * 1000 * 1000)
  job
  runScheduled job pauseTimeMinutes
