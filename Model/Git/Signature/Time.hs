module Model.Git.Signature.Time where

import Control.Applicative
import Control.Monad.IO.Class
import Data.Time (UTCTime)
import qualified Data.Time as Time (getCurrentTime)
import Data.Time.LocalTime (TimeZone(..), getTimeZone)

import Model.Git.Internal


data Time = Time { utc :: UTCTime
                 , offset :: Int
                 }
  deriving (Show)

getCurrentLocalTime :: MonadIO m => Git m Time
getCurrentLocalTime = liftIO $ do
  now    <- Time.getCurrentTime
  offset <- toSeconds . timeZoneMinutes <$> getTimeZone now
  return $ Time now offset
  where
    toSeconds = (* 60)

getCurrentTime :: (Functor m, MonadIO m) => Git m Time
getCurrentTime = flip Time 0 <$> liftIO Time.getCurrentTime
