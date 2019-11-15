module Console
  ( Console(..)
  )
where

import           Data.Text                      ( Text
                                                , unpack
                                                )
import           AppM
import           Control.Monad.IO.Class         ( liftIO )

class Console m where
  writeOut :: Text -> m ()

instance Console AppM where
  writeOut = liftIO . putStrLn . unpack

