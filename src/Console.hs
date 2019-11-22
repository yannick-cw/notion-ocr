module Console
  ( Console(..)
  )
where

import           Data.Text                      ( Text
                                                , unpack
                                                )
import           AppM
import           CliParser
import           Control.Monad.IO.Class         ( liftIO )
import           Control.Monad.Reader           ( asks )
import           Control.Monad                  ( when )

class Console m where
  writeOut :: Text -> m ()
  verboseOut :: Text -> m()

instance Console AppM where
  writeOut = liftIO . putStrLn . unpack
  verboseOut t = do
    isVerbose <- asks verbose
    liftIO $ when isVerbose (putStrLn $ unpack t)

