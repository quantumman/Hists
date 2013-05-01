{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}

module Model.Git.Blob where

import Bindings.Libgit2.Blob
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Foreign
import Foreign.C.String

import Model.Git.Internal
import Model.Git.Oid


blob :: MonadIO m => String -> Git m (Oid Blob)
blob dat = do
  repo <- repository
  liftIO . alloca $ \blobOid           ->
    withCString dat $ \dat'            ->
    withForeignPtr repo $ \repository' -> do
      let size = fromIntegral $ length dat
      r <- c'git_blob_create_frombuffer blobOid repository' (castPtr dat') size
      when (r < 0) raiseError
      Oid <$> newForeignPtr_ blobOid
