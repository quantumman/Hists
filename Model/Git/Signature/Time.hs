module Model.Git.Signature.Time where

import Data.Time (UTCTime)


data Time = Time { utc :: UTCTime
                 , offset :: Int
                 }
  deriving (Show)
