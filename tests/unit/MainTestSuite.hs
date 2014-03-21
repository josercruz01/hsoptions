module Main (
    main
 ) where

import Test.Framework

import UnitTestHelper
import qualified System.Console.HsOptionsTest as HsOptionsTest
import qualified System.Console.HsOptions.ConfParserTest as ConfParserTest

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests =
  [
    unitTestGroup "HsOptions" HsOptionsTest.tests,
    unitTestGroup "ConfParser" ConfParserTest.tests
  ]
