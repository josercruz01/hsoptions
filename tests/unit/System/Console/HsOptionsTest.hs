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
    testMissingOptionalFlag,
    testOptionalFlagMissingValue,
    testOptionalFlagCorrectValue,
    testOptionalFlagIncorrectValue,
    testMissingBoolFlagIsFalse,
    testEmptyBoolFlagIsTrue
  ]

{- Flags -}
userId :: HSO.Flag Int
userId = HSO.make ("user_id", "user_id_help", [HSO.parser HSO.intParser])

userName :: HSO.Flag String
userName = HSO.make ("user_name", "user_name_help", [HSO.parser HSO.stringParser])

help :: HSO.Flag (Maybe Bool)
help = HSO.make ("help", "help_helptext", [HSO.maybeParser HSO.boolParser, 
                                           HSO.isOptional])

dryRun :: HSO.Flag Bool
dryRun = HSO.make ("dry_run", "dryrun_helptext", HSO.boolFlag)

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
  do let flagData = makeFlagData [f2d userId, f2d help]
         pr = process flagData "--user_id 123"
     assertFlagValueEquals pr help Nothing

testOptionalFlagMissingValue :: UnitTest
testOptionalFlagMissingValue = "An optional flag without value should report error" `unitTest`
  do let flagData = makeFlagData [f2d userId, f2d help]
         pr = process flagData "--user_id 123 --help"
     assertNonFatalError pr "Error with flag '--help': Flag value was not provided"
     assertSingleError pr

testOptionalFlagCorrectValue :: UnitTest
testOptionalFlagCorrectValue = "An optional flag with correct value should work correctly" `unitTest`
  do let flagData = makeFlagData [f2d userId, f2d help]
         pr = process flagData "--user_id 123 --help True"
     assertFlagValueEquals pr help (Just True)
     assertFlagValueEquals pr userId 123

testOptionalFlagIncorrectValue :: UnitTest
testOptionalFlagIncorrectValue = "An optional flag with incorrect value should report error" `unitTest`
  do let flagData = makeFlagData [f2d userId, f2d help]
         pr = process flagData "--user_id 123 --help blah"
     assertNonFatalError pr "Error with flag '--help': Value 'blah' is not valid"
     assertSingleError pr

testMissingBoolFlagIsFalse :: UnitTest
testMissingBoolFlagIsFalse = "An missing boolean flag defaults to False" `unitTest`
  do let flagData = makeFlagData [f2d userId, f2d dryRun]
         pr = process flagData "--user_id 123 "
     assertFlagValueEquals pr dryRun False
     assertFlagValueEquals pr userId 123

testEmptyBoolFlagIsTrue :: UnitTest
testEmptyBoolFlagIsTrue = "An boolean flag with empty value defaults should be True" `unitTest`
  do let flagData = makeFlagData [f2d userId, f2d dryRun]
         pr = process flagData "--user_id 123 --dry_run"
     assertFlagValueEquals pr dryRun True
     assertFlagValueEquals pr userId 123

