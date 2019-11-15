{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
module Notion where

import           Data.Text                      ( Text
                                                , unpack
                                                )
import           Data.UUID                      ( UUID
                                                , fromString
                                                )
import           AppM

import           Network.Wreq
import           Control.Monad.Reader
import           Control.Monad.Except
import           Data.Aeson.Types               ( ToJSON
                                                , FromJSON(..)
                                                , emptyObject
                                                )
import           Data.Aeson                     ( toJSON )
import           Data.Aeson.Lens                ( key
                                                , _Object
                                                )
import           Data.Maybe                     ( maybeToList
                                                , listToMaybe
                                                )
import           Data.Map                      as M
                                                ( Map
                                                , lookup
                                                )
import           Data.HashMap.Strict            ( keys )
import           Control.Lens
import           GHC.Generics
import           CliParser
import           Util.Utils
import           Safe                           ( lastMay )


notionUrl :: String
notionUrl = "https://www.notion.so/api/v3/"

data NotionSearchRes = NotionSearchRes { imageURL :: Text, insertId :: UUID }

class Notion m where
  searchNotion :: m [NotionSearchRes]
  insertOCR :: Text -> UUID -> m ()

instance Notion AppM where
  searchNotion = pure []
  insertOCR _ _ = pure ()

data SearchResult = SearchResult { results :: [UUID], recordMap :: RecordMap } deriving (Generic, Show)
instance FromJSON SearchResult
newtype RecordMap = RecordMap { block :: Map UUID Entry} deriving (Generic, Show)
instance FromJSON RecordMap
newtype Entry = Entry { value :: Value}deriving (Generic, Show)
instance FromJSON Entry
data Value = Value { content :: Maybe [UUID], parent_id :: UUID} deriving (Generic, Show)
instance FromJSON Value

data SearchQuery = SearchQuery { query :: Text, table :: Text, id :: UUID, limit :: Int} deriving (Generic, Show)
instance ToJSON SearchQuery

testSearch :: IO ()
testSearch = (putStr . show) =<< runReaderT
  (runExceptT search)
  (Args
    "d493ddb9827dcc5404827319b8053d265bc5ccd9cc17497feef76d42f137de91d512e186b54c2268ac98daf3522fb530f6a2b365d083a3132b6e28baba309009c0c2aa05cb364a71f8a754892677"
    ""
    False
  )

data FoundImage = FoundImage { imageId :: UUID, matchId :: UUID } deriving (Show)
search :: HasNotion r => ExceptT Text (ReaderT r IO) [FoundImage]
search = do
  userSpaceId <- loadUserSpace
  opts        <- lift cookieOpts
  r           <- liftIO $ postWith
    opts
    (notionUrl ++ "searchBlocks")
    (toJSON $ SearchQuery "add_ocr" "space" userSpaceId 1000)
  response <- asJSON r
  let pageRes         = response ^. responseBody
      (ids, blockMap) = (results pageRes, block $ recordMap pageRes)
  return $ ids >>= (maybeToList . findImage blockMap)
 where
  findImage recMap match = do
    searchMatchBlock <- M.lookup match recMap
    let parentId = parent_id $ value searchMatchBlock
    parentBlock <- M.lookup parentId recMap
    image <- content (value parentBlock) >>= (lastMay . takeWhile (/= match))
    return $ FoundImage { imageId = image, matchId = match }


loadUserSpace :: HasNotion r => ExceptT Text (ReaderT r IO) UUID
loadUserSpace = do
  opts <- lift cookieOpts
  r    <- liftIO $ postWith opts (notionUrl ++ "loadUserContent") emptyObject
  let maybeId =
        r
          ^?  responseBody
          .   key "recordMap"
          .   key "space"
          .   _Object
          >>= listToMaybe
          .   keys
          >>= fromString
          .   unpack
  case maybeId of
    Just j  -> return j
    Nothing -> throwError
      "Notion responded not with expected Json, .recordMap.space.{id}"
