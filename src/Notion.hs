module Notion where

import           Data.Text                      ( Text )
import           Data.UUID                      ( UUID )
import           AppM

data NotionSearchRes = NotionSearchRes { imageURL :: Text, insertId :: UUID }

class Notion m where
  searchNotion :: m [NotionSearchRes]
  insertOCR :: Text -> UUID -> m ()

instance Notion AppM where
  searchNotion = pure []
  insertOCR _ _ = pure ()


