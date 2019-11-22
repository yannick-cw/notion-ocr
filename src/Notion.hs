{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
module Notion
  ( Notion(..)
  , NotionSearchRes(..)
  )
where

import           Data.Text                     as T
                                                ( Text
                                                , unpack
                                                , pack
                                                , append
                                                )
import           Data.UUID                      ( UUID
                                                , fromString
                                                , toString
                                                )
import           Data.ByteString.Lazy.Char8    as C
                                                ( unpack )
import           AppM
import           Console
import           Network.Wreq
import           Control.Monad.Reader
import           Control.Monad.Except
import           Data.Aeson.Types               ( ToJSON
                                                , FromJSON(..)
                                                , emptyObject
                                                )
import           Data.ByteString.Lazy.Internal  ( ByteString )
import           Data.Aeson                     ( toJSON )
import           Data.Aeson.Encode.Pretty       ( encodePretty )
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
  searchNotion = search
  insertOCR    = insertOcr

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

search :: AppM [NotionSearchRes]
search = loadUserSpace >>= searchImageId >>= witherM
  (\case
    (FoundImage imId mId) -> catchError
      (Just . NotionSearchRes mId <$> loadImageUrl imId)
      (\err -> liftIO $ putStrLn (T.unpack err) $> Nothing)
  )

searchImageId :: UUID -> AppM [FoundImage]
searchImageId userSpaceId = do
  opts <- AppM $ lift cookieOpts
  let url   = notionUrl ++ "searchBlocks"
      sBody = toJSON $ SearchQuery "add_ocr" "space" userSpaceId 1000
  logRequest url sBody
  r <- liftIO $ postWith opts url sBody
  logResponse r
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


loadUserSpace :: AppM UUID
loadUserSpace = do
  opts <- AppM $ lift cookieOpts
  let url = notionUrl ++ "loadUserContent"
  logRequest url emptyObject
  r <- liftIO $ postWith opts url emptyObject
  logResponse r
  let maybeId =
        r
          ^?  responseBody
          .   key "recordMap"
          .   key "space"
          .   _Object
          >>= headMay
          .   keys
          >>= fromString
          .   T.unpack
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

loadImageUrl :: UUID -> AppM Text
loadImageUrl imageRecordId = do
  opts <- AppM $ lift cookieOpts
  let url   = notionUrl ++ "getRecordValues"
      jBody = toJSON $ RecordReq [Request "block" (toString imageRecordId)]
  logRequest url jBody
  r <- liftIO $ postWith opts url jBody
  logResponse r
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

insertOcr :: Text -> UUID -> AppM ()
insertOcr text insertIntoId = do
  opts <- AppM $ lift cookieOpts
  let transaction = toJSON $ Transaction { operations = contentPart }
      contentPart = [addContent insertIntoId text]
  let url = notionUrl ++ "submitTransaction"
  logRequest url transaction
  r <- liftIO $ postWith opts url transaction
  logResponse r
 where
  addContent opId opContent = Operation { Notion.id = toString opId
                                        , path      = ["properties", "title"]
                                        , command   = "set"
                                        , table     = "block"
                                        , args      = [[opContent]]
                                        }


logRequest :: ToJSON a => String -> a -> AppM ()
logRequest url body = verboseOut (pack $ "Request Url: " ++ url)
  *> verboseOut (pack $ "Request Body: " ++ C.unpack (encodePretty body))

logResponse :: Response ByteString -> AppM ()
logResponse res = verboseOut (pack $ "Response Status Code: " ++ code)
  *> verboseOut (pack $ "Response Body: " ++ body)
 where
  code = show $ res ^. responseStatus
  body = C.unpack $ res ^. responseBody
