{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
module Notion
  ( Notion(..)
  , NotionSearchRes(..)
  )
where

import           Data.Text                      ( Text
                                                , unpack
                                                , pack
                                                , append
                                                )
import           Data.UUID                      ( UUID
                                                , fromString
                                                , toString
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
import           Data.Maybe                     ( maybeToList )
import           Data.Map                      as M
                                                ( Map
                                                , lookup
                                                )
import           Data.Witherable                ( witherM )
import           Data.HashMap.Strict            ( keys )
import           Data.Functor                   ( ($>) )
import           Control.Lens
import           GHC.Generics
import           CliParser
import           Util.Utils
import           Safe                           ( lastMay
                                                , headMay
                                                )


notionUrl :: String
notionUrl = "https://www.notion.so/api/v3/"

data NotionSearchRes = NotionSearchRes { insertId :: UUID ,  imageURL :: Text } deriving ( Show)

class Notion m where
  searchNotion :: m [NotionSearchRes]
  insertOCR :: Text -> UUID -> m ()

instance Notion AppM where
  searchNotion = AppM search
  insertOCR ocrContent intoId = AppM (insertOcr ocrContent intoId)

data SearchResult = SearchResult { results :: [UUID], recordMap :: Maybe RecordMap } deriving (Generic, Show)
instance FromJSON SearchResult
newtype RecordMap = RecordMap { block :: Maybe (Map UUID Entry) } deriving (Generic, Show)
instance FromJSON RecordMap
newtype Entry = Entry { value :: Value } deriving (Generic, Show)
instance FromJSON Entry
data Value = Value { content :: Maybe [UUID], parent_id :: UUID, properties :: Maybe Source} deriving (Generic, Show)
instance FromJSON Value
newtype Source = Source { source :: Maybe [[Text]]} deriving (Generic, Show)
instance FromJSON Source

data SearchQuery = SearchQuery { query :: Text, table :: Text, id :: UUID, limit :: Int} deriving (Generic, Show)
instance ToJSON SearchQuery

data FoundImage = FoundImage { imageId :: UUID, matchId :: UUID } deriving (Show)

search :: HasNotion r => ExceptT Text (ReaderT r IO) [NotionSearchRes]
search = loadUserSpace >>= searchImageId >>= witherM
  (\case
    (FoundImage imId mId) -> catchError
      (Just . NotionSearchRes mId <$> loadImageUrl imId)
      (\err -> liftIO $ putStrLn (unpack err) $> Nothing)
  )

searchImageId :: HasNotion r => UUID -> ExceptT Text (ReaderT r IO) [FoundImage]
searchImageId userSpaceId = do
  opts <- lift cookieOpts
  r    <- liftIO $ postWith
    opts
    (notionUrl ++ "searchBlocks")
    (toJSON $ SearchQuery "add_ocr" "space" userSpaceId 1000)
  response <- asJSON r
  let pageRes = response ^. responseBody
      (ids, blockMap) =
        ( (results :: SearchResult -> [UUID]) pageRes
        , block =<< recordMap pageRes
        )
  return $ ids >>= (maybeToList . (blockMap >>=) . findImage)
 where
  findImage match recMap = do
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
          >>= headMay
          .   keys
          >>= fromString
          .   unpack
  case maybeId of
    Just j  -> return j
    Nothing -> throwError
      "Notion responded not with expected Json, .recordMap.space.{id}"

data Request = Request { table :: String, id :: String} deriving (Generic)
instance ToJSON Request
newtype RecordReq = RecordReq { requests :: [ Request ]} deriving (Generic)
instance ToJSON RecordReq

newtype RecordResponse = RecordResponse { results :: [Entry]} deriving (Generic, Show)
instance FromJSON RecordResponse

loadImageUrl :: HasNotion r => UUID -> ExceptT Text (ReaderT r IO) Text
loadImageUrl imageRecordId = do
  opts <- lift cookieOpts
  r    <- liftIO $ postWith
    opts
    (notionUrl ++ "getRecordValues")
    (toJSON $ RecordReq [Request "block" (toString imageRecordId)])
  response <- asJSON r
  let pageRes = response ^. responseBody
      maybeImageUrl =
        headMay =<< headMay =<< source =<< (properties . value) =<< headMay
          ((results :: RecordResponse -> [Entry]) pageRes)
  case maybeImageUrl of
    Just j -> return j
    Nothing ->
      throwError
        $        "Did not find Image for record with id "
        `append` pack (toString imageRecordId)
        `append` " aborting image"


data Operation = Operation { id :: String, path :: [String], command :: String, table :: String, args :: [[Text]] } deriving (Generic)
instance ToJSON Operation

newtype Transaction = Transaction { operations :: [ Operation ] } deriving (Generic)
instance ToJSON Transaction

insertOcr :: HasNotion r => Text -> UUID -> ExceptT Text (ReaderT r IO) ()
insertOcr text insertIntoId = do
  opts <- lift cookieOpts
  let transaction = Transaction { operations = contentPart }
      contentPart = [addContent insertIntoId text]
  void $ liftIO $ postWith opts
                           (notionUrl ++ "submitTransaction")
                           (toJSON transaction)
 where
  addContent opId opContent = Operation { Notion.id = toString opId
                                        , path      = ["properties", "title"]
                                        , command   = "set"
                                        , table     = "block"
                                        , args      = [[opContent]]
                                        }
