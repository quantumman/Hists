module Model.Git.Signature
       ( module GitTime
       , Signature
       , Email
       , Name
       , sign
       )
       where

import Bindings.Libgit2.Signature
import Bindings.Libgit2.Types
import Control.Monad
import Control.Monad.IO.Class
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Foreign
import Foreign.C.String

import Model.Git.Internal
import Model.Git.Signature.Time as GitTime


type Signature = ForeignPtr C'git_signature

type Email = String

type Name  = String

sign :: MonadIO m => Name -> Email -> Time -> Git m Signature
sign name email time = liftIO $ do
  alloca $ \signature ->
    withCString name $ \name'   ->
    withCString email $ \email' -> do
      r <- c'git_signature_new signature
                               name'
                               email'
                               utcSeconds
                               utcOffset
      when (r < 0) raiseError
      peek signature >>= newForeignPtr p'git_signature_free
  where
    utcSeconds = fromIntegral . round . utcTimeToPOSIXSeconds . utc $ time

    utcOffset = fromIntegral . offset $ time
