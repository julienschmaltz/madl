module Main where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit

import qualified Madl.Network (unitTests)
import qualified Madl.Base (unitTests)
import qualified Madl.Islands (unitTests)

main :: IO ()
main = defaultMain tests

tests :: [Test.Framework.Test]
tests = (hUnitTestToTests . TestList) unitTests

unitTests :: [Test.HUnit.Test]
unitTests =
        Madl.Network.unitTests
    ++  Madl.Base.unitTests
    ++  Madl.Islands.unitTests