{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
module Notion where

import           Data.Text                      ( Text )
import           Data.UUID                      ( UUID )
import           AppM

import           Network.Wreq
import           Network.HTTP.Client            ( createCookieJar
                                                , Cookie(Cookie)
                                                )
import           Control.Monad.Reader
import           Control.Monad.Except
import           Data.Aeson.Types               ( emptyObject
                                                , ToJSON
                                                , FromJSON(..)
                                                , sumEncoding
                                                , unwrapUnaryRecords
                                                , genericToJSON
                                                , fieldLabelModifier
                                                , defaultOptions
                                                , SumEncoding(..)
                                                )
import           Data.Aeson.Lens                ( _Object
                                                , key
                                                )
import           Data.Aeson                     ( toJSON
                                                , genericParseJSON
                                                )
import           Data.Time.Clock
import           Data.ByteString.Char8          ( pack )
import           Data.Text                      ( unpack )
import           Data.Maybe                     ( listToMaybe
                                                , catMaybes
                                                , maybeToList
                                                )
import           Data.UUID
import           Control.Lens
import           Control.Applicative            ( liftA3 )
import           GHC.Generics
import           System.Random                  ( randomIO )


notionUrl :: String
notionUrl = "https://www.notion.so/api/v3/"

data NotionSearchRes = NotionSearchRes { imageURL :: Text, insertId :: UUID }

class Notion m where
  searchNotion :: m [NotionSearchRes]
  insertOCR :: Text -> UUID -> m ()

instance Notion AppM where
  searchNotion = pure []
  insertOCR _ _ = pure ()

