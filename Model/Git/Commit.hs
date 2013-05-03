module Model.Git.Commit
       ( Author
       , Committer
       , Parents
       , UpdateRef
       , Message
       )
       where

import Control.Monad (forM)
import Foreign
import Foreign.C.String

import Model.Git.Signature


type Author = Signature

type Committer = Signature

type Parents = [Oid Commit]

type UpdateRef = String

type Message = String

withForeignPtrs :: [ForeignPtr a] -> ([Ptr a] -> IO b) -> IO b
withForeignPtrs fptrs f = do
  ptrs <- forM fptrs $ flip withForeignPtr return
  f ptrs
