module Tests.Test.Hspec.Fixture where

import Control.Monad ((=<<))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec


describe' :: String -> IO Spec -> IO Spec
describe' msg = (return . describe msg =<<)

fixtureSandbox :: String -> Spec -> IO Spec
fixtureSandbox name = withSystemTempDirectory name . const . return
