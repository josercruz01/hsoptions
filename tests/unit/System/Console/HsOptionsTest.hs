module System.Console.HsOptionsTest where

import UnitTestHelper
import qualified System.Console.HsOptions as HSO
import System.Console.HsOptionsTestHelpers

tests :: [UnitTest]
tests = [
    testValidFlag,
    testInvalidFlagError,
    testFlagWithNoNameError,
    testFlagWithNoNameError2,
    testNegativeNumbers,
    testNegativeDecimals,
    testMissingFlagError,
    testFlagNotDefined,
    testMissingOptionalFlag
  ]

{- Flags -}
userId :: HSO.Flag Int
userId = HSO.make ("user_id", "user_id_help", HSO.intFlag)

userName :: HSO.Flag String
userName = HSO.make ("user_name", "user_name_help", HSO.stringFlag)

help :: HSO.Flag (Maybe Bool)
help = HSO.make ("help", "help_helptext", HSO.maybeFlag HSO.boolFlag)

constraints :: [HSO.FlagConstraint]
constraints = [
    HSO.flagOptional help
  ]

{- Test methods -}

testValidFlag :: UnitTest
testValidFlag  = "Valid flag should be parsed correctly" `unitTest`
  do let flagData = makeFlagData [f2d userId]
         pr = process flagData "--user_id 123"
     assertFlagValueEquals pr userId 123

testMissingFlagError :: UnitTest
testMissingFlagError  = "Missing flag should report error" `unitTest`
  do let flagData = makeFlagData [f2d userId, f2d userName]
         pr = process flagData "--user_id 123"
     assertNonFatalError pr "Error with flag '--user_name': Flag is required"
     assertSingleError pr

testInvalidFlagError :: UnitTest
testInvalidFlagError = "Invalid flag type should report error" `unitTest`
  do let flagData = makeFlagData [f2d userId]
         pr = process flagData "--user_id 123abc"
     assertNonFatalError pr "Error with flag '--user_id': Value '123abc' is not valid"
     assertSingleError pr

testFlagWithNoNameError :: UnitTest
testFlagWithNoNameError = "Two dashes with no name should be considered an argument" `unitTest`
  do let flagData = makeFlagData [f2d userId]
         pr = process flagData "--user_id 123 --"
     assertFlagValueEquals pr userId 123
     assertArgsEquals pr ["--"]

testFlagWithNoNameError2 :: UnitTest
testFlagWithNoNameError2 = "Single dash with no name should be considered an argument" `unitTest`
  do let flagData = makeFlagData [f2d userId]
         pr = process flagData "- --user_id 123"
     assertFlagValueEquals pr userId 123
     assertArgsEquals pr ["-"]

testNegativeNumbers :: UnitTest
testNegativeNumbers = "A negative number should not be treated as a flag" `unitTest`
  do let flagData = makeFlagData [f2d userId]
         pr = process flagData "--user_id 123 -128"
     assertFlagValueEquals pr userId 123
     assertArgsEquals pr ["-128"]

testNegativeDecimals :: UnitTest
testNegativeDecimals = "A negative decimal number should not be treated as a flag" `unitTest`
  do let flagData = makeFlagData [f2d userId]
         pr = process flagData "--user_id 123 -128.18"
     assertFlagValueEquals pr userId 123
     assertArgsEquals pr ["-128.18"]

testFlagNotDefined :: UnitTest
testFlagNotDefined = "A passed in flag not defined in the code should report error" `unitTest`
  do let flagData = makeFlagData [f2d userId]
         pr = process flagData "--user_id 123 --user_name bender"
     assertNonFatalError pr "Error with flag --user_name: Unkown flag is not defined in the code"
     assertSingleError pr

testMissingOptionalFlag :: UnitTest
testMissingOptionalFlag = "A missing optional flag should set to Nothing" `unitTest`
  do let flagData = makeFlagDataConstrained [f2d userId, f2d help] constraints
         pr = process flagData "--user_id 123"
     assertFlagValueEquals pr help Nothing
