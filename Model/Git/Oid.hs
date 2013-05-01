{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}

module Model.Git.Oid where

import Bindings.Libgit2.Oid
import Foreign


data GitObject = Tree
               | Blob
               | Commit
  deriving (Show)

data Oid (a :: GitObject) = Oid (ForeignPtr C'git_oid)
  deriving (Show)
