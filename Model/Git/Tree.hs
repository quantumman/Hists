module Model.Git.Tree where

import Bindings.Libgit2.Types
import Foreign


type TreeBuilder = ForeignPtr C'git_treebuilder
