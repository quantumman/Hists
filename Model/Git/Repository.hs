module Model.Git.Repository
       ( module Repository
       , Repository
       ) where

import Bindings.Libgit2.Repository as Repository
import Bindings.Libgit2.Types as Repository
import Control.Monad
import Foreign
import Foreign.C.String
import Foreign.ForeignPtr
import Foreign.Storable

type Repository = ForeignPtr C'git_repository
