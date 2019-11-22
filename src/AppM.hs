{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}

module AppM where

import           Control.Monad.Except
import           Data.Text
import           Control.Monad.Reader
import           CliParser
import           GHC.IO.Exception               ( ioe_description )
import           Control.Monad.Catch

liftIOErr :: MonadError Text m => IOError -> m a
liftIOErr = throwError . pack . ioe_description

newtype AppM a = AppM { unwrap :: ExceptT Text (ReaderT Args IO) a }
  deriving (
              Functor
            , Applicative
            , Monad
            , MonadReader Args
            , MonadIO
            , MonadError Text
            , MonadThrow
           )
