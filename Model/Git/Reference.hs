module Model.Git.Reference
       ( Reference(..)
       , ReferenceName
       , Model.Git.Reference.lookup
       )
       where

import Bindings.Libgit2.Oid
import Bindings.Libgit2.Refs
import Bindings.Libgit2.Types
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Foreign
import Foreign.C.String

import Model.Git.Internal
import Model.Git.Oid

type ReferenceName = String

data Reference a = Direct (Oid a) | Symbolic ReferenceName

data GitReferenceType = GitReferenceInvalid
                      | GitReferenceOid
                      | GitReferenceSymbolic
                      | GitReferenceListAll
  deriving (Show, Enum)

lookup :: MonadIO m => ReferenceName -> Git m (Reference a)
lookup referenceName = do
  repo <- repository
  lookup' repo >>= resolveType
  where
    lookup' repo = liftIO $
      withForeignPtr repo $ \repository           ->
      withCString referenceName $ \referenceName' ->
      alloca $ \reference                         -> do
        r <- c'git_reference_lookup reference repository referenceName'
        when (r < 0) raiseError
        peek reference >>= newForeignPtr p'git_reference_free

    resolveType reference = liftIO $
      withForeignPtr reference $ \reference' -> do
        referenceType <- toEnum . fromIntegral <$> c'git_reference_type reference'
        case referenceType of
          GitReferenceOid      -> Direct . Oid <$>
                                  (c'git_reference_target reference'
                                   >>= newForeignPtr_)
          GitReferenceSymbolic -> Symbolic <$>
                                  (c'git_reference_symbolic_target reference'
                                   >>= peekCString)
          _                    -> raiseError
