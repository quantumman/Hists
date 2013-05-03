{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}

module Model.Git.Tree where

import Bindings.Libgit2.Tree
import Bindings.Libgit2.Types
import Control.Monad
import Control.Monad.IO.Class
import Foreign
import Foreign.C
import Foreign.C.Types

import Model.Git.Internal
import Model.Git.Oid


type TreeBuilder = ForeignPtr C'git_treebuilder

class GitTreeable (a :: GitObject) where
  filemode :: Oid a -> CUInt

  insert :: (MonadIO m) => TreeBuilder -> String -> Oid a -> Git m ()
  insert builder name oid@(Oid tree) = liftIO $
    withForeignPtr builder $ \builder' ->
    withCString name $ \filename'      ->
    withForeignPtr tree $ \blobOid'    -> do
      r <- c'git_treebuilder_insert nullPtr
                                    builder'
                                    filename'
                                    blobOid'
                                    (filemode oid)
      when (r < 0) raiseError
      return ()

create :: MonadIO m => Git m TreeBuilder
create = liftIO $ do
  alloca $ \ptr -> do
    r <- c'git_treebuilder_create ptr nullPtr
    when (r < 0) raiseError
    ptr' <- peek ptr
    newForeignPtr p'git_treebuilder_free ptr'
