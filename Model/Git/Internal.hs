{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Model.Git.Internal
       ( Git
       , git
       , raiseError
       , repository
       )
       where

import Bindings.Libgit2.Errors
import Bindings.Libgit2.Threads
import Control.Applicative
import Control.Exception (SomeException(..))
import Control.Monad
import Control.Monad.CatchIO (MonadCatchIO(..))
import qualified Control.Monad.CatchIO as CatchIO (catch)
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
           , MonadCatchIO
           , MonadError String
           , MonadReader Repository
           )

git :: (Functor m, MonadCatchIO m, MonadIO m)
       => FilePath
       -> Git m a
       -> m (Either String a)
git filepath action =
  flip runReaderT neverEvaluated . runErrorT $ unGit go `CatchIO.catch` rethrow
  where
    unGit (Git g) = g

    rethrow :: Monad m => SomeException -> m a
    rethrow (SomeException e) = fail . show $ e

    neverEvaluated = undefined

    go = do
      initializeThreads
      repository <- openOrCreate filepath
      local (const repository) action

repository :: MonadIO m => Git m Repository
repository = ask

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

initializeThreads :: MonadIO m => Git m ()
initializeThreads = liftIO $ do
  r <- c'git_threads_init
  when (r < 0) $ raiseError
