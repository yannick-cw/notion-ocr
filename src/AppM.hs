{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module AppM where

import           Control.Monad.Except
import           Data.Text
import           Control.Monad.Reader

newtype Args = Args { notionToken :: Text }
newtype AppM a = AppM { unwrap :: ExceptT Text (ReaderT Args IO) a }
  deriving (
              Functor
            , Applicative
            , Monad
            , MonadReader Args
            , MonadIO
            , MonadError Text
           )
