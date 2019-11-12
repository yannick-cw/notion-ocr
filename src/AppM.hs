{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module AppM where

import           Control.Monad.Except
import           Data.Text
import           Control.Monad.Reader

data Args = Args { notionToken :: Text, tempPath :: FilePath }
class HasTempDir a where
  path :: a  -> FilePath

instance HasTempDir Args where
  path = tempPath

newtype AppM a = AppM { unwrap :: ExceptT Text (ReaderT Args IO) a }
  deriving (
              Functor
            , Applicative
            , Monad
            , MonadReader Args
            , MonadIO
            , MonadError Text
           )
