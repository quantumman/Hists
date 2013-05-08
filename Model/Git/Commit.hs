{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Model.Git.Commit
       ( Author
       , Committer
       , Parents
       , UpdateRef
       , Message
       , file
       )
       where

import Bindings.Libgit2.Commit
import Bindings.Libgit2.Tree
import Bindings.Libgit2.Types
import Control.Applicative
import Control.Exception (bracket)
import Control.Monad (forM, forM_, when)
import Control.Monad.IO.Class
import Data.Functor
import Data.List (dropWhileEnd)
import Data.Text (Text, unpack)
import Foreign
import Foreign.C.String
import qualified System.Directory.Layout as Layout (file)
import System.Directory.Layout.Internal
import System.FilePath

import Model.Git.Blob
import Model.Git.Internal
import Model.Git.Oid
import Model.Git.Repository
import Model.Git.Signature
import Model.Git.Tree


type Author = Signature

type Committer = Signature

type Parents = [Oid Commit]

type UpdateRef = String

type Message = String

commit' :: MonadIO m
           => Oid Tree
           -> Author
           -> Committer
           -> Parents
           -> Maybe UpdateRef
           -> Message
           -> Git m (Oid Commit)
commit' treeOid author committer parents updateRef message = do
  repo <- repository
  tree <- lookupTree repo treeOid
  liftIO . alloca $ \ptr ->
    withForeignPtr repo $ \repository'            ->
    withForeignPtr tree $ \tree'                  ->
    withForeignPtr author $ \author'              ->
    withForeignPtr committer $ \committer'        ->
    withParents repo parents $ \(size, parents')  ->
    withUpdateRef updateRef $ \updateRef'         ->
    withCString message $ \message'               -> do
      r <- c'git_commit_create ptr
                               repository'
                               updateRef'
                               author'
                               committer'
                               utf8
                               message'
                               tree'
                               size
                               parents'
      when (r < 0) raiseError
      Oid <$> newForeignPtr_ ptr
  where
    lookupTree repository (Oid treeOid) = liftIO $
      withForeignPtr repository $ \repository' ->
      withForeignPtr treeOid $ \treeOid'       ->
      alloca $ \tree -> do
        r <- c'git_tree_lookup tree repository' treeOid'
        when (r < 0) raiseError
        tree' <- peek tree
        newForeignPtr_ tree'

    withUpdateRef updateRef f = case updateRef of
      Nothing  -> f nullPtr
      Just ref -> withCString ref f

    utf8 = nullPtr

    withParents repo parents f =
      withCommits repo parents $ \parents' -> do
        let size = length parents'
        liftIO $ allocaArray size $ \array -> do
          pokeArray array parents'
          f (fromIntegral size, array)

withCommits :: Repository
               -> Parents
               -> ([Ptr C'git_commit] -> IO a)
               -> IO a
withCommits repo parents f = do
  bracket
    (lookupCommits repo parents)
    (\cs -> liftIO $ forM_ cs c'git_commit_free)
    f
  where
    unOid (Oid o) = o

    lookupCommits repo parents = liftIO $
      withForeignPtr repo $ \repo'                ->
      withForeignPtrs (map unOid parents) $ \oids ->
      forM oids $ \oid -> alloca $ \commit        -> do
        r <- c'git_commit_lookup commit repo' oid
        when (r < 0) raiseError
        peek commit

withForeignPtrs :: [ForeignPtr a] -> ([Ptr a] -> IO b) -> IO b
withForeignPtrs fptrs f = do
  ptrs <- forM fptrs $ flip withForeignPtr return
  f ptrs

makeTree :: (Functor m, MonadIO m) => Layout -> Git m (Oid Tree)
makeTree layout = do
  treeBuilder <- create
  makeTrees layout >>= mapM_ (insert' treeBuilder)
  write treeBuilder
  where
    insert' tb = either (uncurry $ insert tb) (uncurry $ insert tb)

    makeTrees :: (Functor m, MonadIO m)
                 => Layout
                 -> Git m [Either (FilePath, Oid Blob) (FilePath, Oid Tree)]
    makeTrees (E _) = return []
    makeTrees (F filePath (E _) x)   = return []
    makeTrees (F filePath (T t _) x) = do
      blobs <- makeTrees x
      blob <- Left <$> (,) filePath <$> blob (unpack t)
      return $ blob:blobs
    makeTrees (D filePath x y)       = do
      treeBuilder <- create
      makeTrees x >>= mapM_ (insert' treeBuilder)
      trees <- makeTrees y
      tree <- Right <$> (,) filePath <$> write treeBuilder
      return $ tree:trees

file :: FilePath -> Text -> Layout
file = Layout.file
