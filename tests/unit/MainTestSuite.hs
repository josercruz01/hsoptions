module Main (
    main
 ) where

import Test.Framework

import UnitTestHelper
import qualified System.Console.HsOptionsTest as HsOptionsTest

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests =
  [
    unitTestGroup "HsOptions" HsOptionsTest.tests
  ]
