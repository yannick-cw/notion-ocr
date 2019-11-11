module Console
  ( Console(..)
  )
where

import           Data.Text                      ( Text )
import           AppM

class Console m where
  writeOut :: Text -> m ()

instance Console AppM where
  writeOut _ = pure ()

