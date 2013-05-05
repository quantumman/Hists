{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}

module Model.Git.Tree where

import Bindings.Libgit2.Tree
import Bindings.Libgit2.Types
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Foreign
import Foreign.C
import Foreign.C.Types

import Model.Git.Internal
import Model.Git.Oid (Oid(..))
import qualified Model.Git.Oid as Oid (GitObject(..))


type TreeBuilder = ForeignPtr C'git_treebuilder

class GitTreeable (a :: Oid.GitObject) where
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

instance GitTreeable Oid.Tree where
  filemode = const 0o040000

instance GitTreeable Oid.Blob where
  filemode = const 0o100644


create :: MonadIO m => Git m TreeBuilder
create = liftIO $ do
  alloca $ \ptr -> do
    r <- c'git_treebuilder_create ptr nullPtr
    when (r < 0) raiseError
    ptr' <- peek ptr
    newForeignPtr p'git_treebuilder_free ptr'

write :: MonadIO m => TreeBuilder -> Git m (Oid Oid.Tree)
write builder = do
  repo <- repository
  liftIO $ withForeignPtr repo $ \repository' ->
    withForeignPtr builder $ \builder'              ->
    alloca $ \oid -> do
      r <- c'git_treebuilder_write oid repository' builder'
      when (r < 0) raiseError
      Oid <$> newForeignPtr_ oid
