module System.Console.HsOptionsTest where

import UnitTestHelper
import qualified System.Console.HsOptions as HSO
import System.Console.HsOptionsTestHelpers

tests :: [UnitTest]
tests = [
    testValidFlag,
    testMissingFlagError
  ]

{- Flags -}
userId :: HSO.Flag Int
userId = HSO.make ("user_id", "user_id_help", HSO.intFlag)

userName :: HSO.Flag String
userName = HSO.make ("user_name", "user_name_help", HSO.stringFlag)

{- Test methods -}

testValidFlag :: UnitTest
testValidFlag  = "Valid flag should be parsed correctly" `unitTest`
  do let flagData = makeFlagData [f2d userId]
         pr = process flagData "--user_id 123"
     assertFlagValueEquals pr userId 1233


testMissingFlagError :: UnitTest
testMissingFlagError = "Invalid flag type should report error" `unitTest`
  do let flagData = makeFlagData [f2d userId]
         pr = process flagData "--user_id 123abc"
     assertNonFatalError pr "Value '123abc' for flag '--user_id' is invalid"
     assertSingleError pr
