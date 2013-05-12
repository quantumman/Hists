module Tests.Test.Hspec.Fixture where

import Control.Monad ((=<<))
import Test.Hspec


describe' :: String -> IO Spec -> IO Spec
describe' msg = (return . describe msg =<<)
