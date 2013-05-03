module Model.Git.Signature
       ( Signature
       )
       where

import Bindings.Libgit2.Types
import Foreign


type Signature = ForeignPtr C'git_signature
