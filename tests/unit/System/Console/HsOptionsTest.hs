module System.Console.HsOptionsTest where

import Test.HUnit
import UnitTestHelper
import Data.Maybe
import qualified System.Console.HsOptions as HSO
import qualified Data.Map as Map

tests :: [UnitTest]
tests = [
    testMakeFlag,
    testProcessSingleFlag
  ]

{- Helper methods -}
mockIntFlag :: String -> Maybe Int
mockIntFlag _ = Just 1234

userIdFlag :: HSO.Flag Int
userIdFlag = HSO.make("user_id", "user_id help", HSO.intFlag)

fromRight :: Either a b -> b
fromRight (Right b) = b
fromRight _ = error "Cant' take Right of a Left Either"

assertContains :: String -> Either [HSO.FlagError] HSO.ProcessResults ->  (String, String) -> Assertion
assertContains testName processResults expected = assertEqual testName expectedValue currentValue
    where (flagResults, _argResults) = fromRight processResults
          (key, expectedValue) = expected
          currentValue = fromJust (Map.lookup key flagResults)

{- Test methods -}
testMakeFlag :: UnitTest
testMakeFlag =  "HsOptions.make should create a basic flag " `unitTest`
  do let HSO.Flag name help parser = HSO.make ("blahName", "blahHelp", mockIntFlag)
     assertEqual "Name of the flag should be saved" "blahName" name
     assertEqual "Help of the flag should be saved" "blahHelp" help
     assertEqual "Flag value parser should be saved" (Just 1234) (parser "")

testProcessSingleFlag :: UnitTest
testProcessSingleFlag  = "HsOptions.process should parse a single flag" `unitTest`
  do let flagData = HSO.flagToData userIdFlag
         processResult = HSO.process flagData ["--user_id", "123"]
     assertContains "The flag value should be saved in the result file" processResult ("user_id", "123")
