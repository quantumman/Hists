{-# LANGUAGE DataKinds #-}

module Model.Git.Commit
       ( Author
       , Committer
       , Parents
       , UpdateRef
       , Message
       )
       where

import Bindings.Libgit2.Commit
import Bindings.Libgit2.Types
import Control.Applicative
import Control.Exception (bracket)
import Control.Monad (forM, forM_, when)
import Control.Monad.IO.Class
import Foreign
import Foreign.C.String

import Model.Git.Internal
import Model.Git.Oid
import Model.Git.Repository
import Model.Git.Signature


type Author = Signature

type Committer = Signature

type Parents = [Oid Commit]

type UpdateRef = String

type Message = String

withCommits :: Repository
               -> Parents
               -> ([Ptr C'git_commit] -> IO a)
               -> IO a
withCommits repo parents f = do
  bracket
    (lookupCommits repo parents)
    (\cs -> liftIO $ forM_ cs c'git_commit_free)
    f
  where
    unOid (Oid o) = o

    lookupCommits repo parents = liftIO $
      withForeignPtr repo $ \repo'                ->
      withForeignPtrs (map unOid parents) $ \oids ->
      forM oids $ \oid -> alloca $ \commit        -> do
        r <- c'git_commit_lookup commit repo' oid
        when (r < 0) raiseError
        peek commit

withForeignPtrs :: [ForeignPtr a] -> ([Ptr a] -> IO b) -> IO b
withForeignPtrs fptrs f = do
  ptrs <- forM fptrs $ flip withForeignPtr return
  f ptrs
