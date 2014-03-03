module System.Console.HsOptionsTest where

import Test.HUnit
import UnitTestHelper
import qualified System.Console.HsOptions as HSO

tests :: [UnitTest]
tests = [
    testMakeFlag
  ]

mockIntFlag :: String -> Maybe Int
mockIntFlag _ = Just 1234

testMakeFlag :: UnitTest
testMakeFlag =  "HsOptions.make should create a basic flag " `unitTest`
  do let HSO.Flag name help parser = HSO.make ("blahName", "blahHelp", mockIntFlag)
     assertEqual "Name of the flag should be saved" "blahName" name
     assertEqual "Help of the flag should be saved" "blahHelp" help
     assertEqual "Flag value parser should be saved" (Just 1234) (parser "")

testProcess_singleFlag :: UnitTest
testProcess_singleFlag  = "HsOptions.process should process a single flag" `unitTest`
  do let
