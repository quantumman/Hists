module Model.Git.Commit where

import Foreign
import Control.Monad


withForeignPtrs :: [ForeignPtr a] -> ([Ptr a] -> IO b) -> IO b
withForeignPtrs fptrs f = do
  ptrs <- forM fptrs $ flip withForeignPtr return
  f ptrs
