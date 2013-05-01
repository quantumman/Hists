{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Model.Git.Internal
       ( Git
       , raiseError
       )
       where

import Bindings.Libgit2.Errors
import Control.Applicative
import Control.Monad
import Control.Monad.Error
import Control.Monad.Reader
import Foreign
import Foreign.C.String

import Model.Git.Repository


newtype Git m a = Git (ErrorT String (ReaderT Repository m) a)
  deriving ( Functor
           , Applicative
           , Monad
           , MonadIO
           , MonadError String
           , MonadReader Repository
           )

raiseError :: MonadIO m => m a
raiseError = liftIO $ do
  msg  <- c'giterr_last
  msg' <- maybePeek peek msg
  fail =<< case msg' of
    Nothing  -> return "unknow error"
    Just err -> peekCString $ c'git_error'message err

openOrCreate :: MonadIO m => FilePath -> Git m Repository
openOrCreate path = liftIO $ do
  withCString path $ \path' ->
    alloca $ \repository -> do
      r <- c'git_repository_open repository path'
      when (r < 0) $ do
        r <- c'git_repository_init repository path' ctrue
        when (r < 0) raiseError
      repository' <- peek repository
      newForeignPtr p'git_repository_free repository'
  where
    ctrue = fromIntegral 1
