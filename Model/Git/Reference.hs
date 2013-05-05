module Model.Git.Reference
       ( Reference(..)
       , ReferenceName
       )
       where

import Foreign

import Model.Git.Oid

type ReferenceName = String

data Reference a = Direct (Oid a) | Symbolic ReferenceName

data GitReferenceType = GitReferenceInvalid
                      | GitReferenceOid
                      | GitReferenceSymbolic
                      | GitReferenceListAll
  deriving (Show, Enum)
