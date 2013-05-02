module Model.Git.Tree where

import Bindings.Libgit2.Tree
import Bindings.Libgit2.Types
import Control.Monad
import Control.Monad.IO.Class
import Foreign

import Model.Git.Internal


type TreeBuilder = ForeignPtr C'git_treebuilder

create :: MonadIO m => Git m TreeBuilder
create = liftIO $ do
  alloca $ \ptr -> do
    r <- c'git_treebuilder_create ptr nullPtr
    when (r < 0) raiseError
    ptr' <- peek ptr
    newForeignPtr p'git_treebuilder_free ptr'
