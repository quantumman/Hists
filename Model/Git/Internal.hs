{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Model.Git.Internal (Git) where

import Control.Applicative
import Control.Monad
import Control.Monad.Error
import Control.Monad.Reader

import Model.Git.Repository


newtype Git m a = Git (ErrorT String (ReaderT Repository m) a)
  deriving ( Functor
           , Applicative
           , Monad
           , MonadIO
           , MonadError String
           , MonadReader Repository
           )
