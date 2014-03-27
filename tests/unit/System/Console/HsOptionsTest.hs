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
    testEmptyBoolFlagIsTrue,
    testRequiredIfNotRequired,
    testRequiredIfRequired,
    testRequiredIfRequiredButProvided,
    testRequiredIfRequiredButMissing,
    testGlobalValidationOccursAtTheEnd,
    testOperationEquals,
    testOperationAppend,
    testOrderOfFlags,
    testOrderOfArgs,
    testFlagOperationWhitespace1,
    testFlagOperationWhitespace2,
    testFlagOperationWhitespace3,
    testFlagOperationWhitespace4,
    testFlagOperationWhitespace5,
    testFlagOperationWhitespace6,
    testFlagOperationWhitespace7,
    testHelpKeywordReserved,
    testHelpKeywordReservedOnAlias,
    testUsingfFileKeywordReserved,
    testEmptyFlagFollowedByFlag,
    testFlagAlias,
    testFlagAliasIncorrectValue
  ]

{- Flags -}
userId :: HSO.Flag Int
userId = HSO.make ("user_id", "user_id_help", [HSO.parser HSO.intParser,
                                               HSO.aliasIs ["u", "uid"]])

userName :: HSO.Flag String
userName = HSO.make ("user_name", "user_name_help", [HSO.parser HSO.stringParser])

help :: HSO.Flag (Maybe Bool)
help = HSO.make ("help", "help_helptext", [HSO.maybeParser HSO.boolParser, 
                                           HSO.isOptional])

helpOnAlias :: HSO.Flag (Maybe Bool)
helpOnAlias = HSO.make ("not_help", "help_helptext", [HSO.maybeParser HSO.boolParser, 
                                                      HSO.isOptional,
                                                      HSO.aliasIs ["help"]])


usingFileFlag :: HSO.Flag String
usingFileFlag  = HSO.make ("usingFile", "usingFile_helptext", [HSO.parser HSO.stringParser])

customHelp :: HSO.Flag (Maybe Bool)
customHelp = HSO.make ("custom_help", "customHelp_helptext", [HSO.maybeParser HSO.boolParser, 
                                           HSO.isOptional])

userLastName :: HSO.Flag (Maybe String)
userLastName = HSO.make ("user_last_name", 
                         "user_last_name_help", 
                         [HSO.maybeParser HSO.stringParser, 
                          HSO.isOptional])

database :: HSO.Flag (Maybe String)
database = HSO.make ("database", 
                     "database_help", 
                     [HSO.maybeParser HSO.stringParser,
                      HSO.requiredIf (\ fr -> HSO.get fr userId == 4444)]
                     )

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
     assertError pr "Error with flag '--user_name': Flag is required"
     assertSingleError pr

testInvalidFlagError :: UnitTest
testInvalidFlagError = "Invalid flag type should report error" `unitTest`
  do let flagData = makeFlagData [f2d userId]
         pr = process flagData "--user_id 123abc"
     assertError pr "Error with flag '--user_id': Value '123abc' is not valid"
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
     assertError pr "Error with flag '--user_name': Unkown flag is not defined in the code"
     assertSingleError pr

testMissingOptionalFlag :: UnitTest
testMissingOptionalFlag = "A missing optional flag should set to Nothing" `unitTest`
  do let flagData = makeFlagData [f2d userId, f2d customHelp]
         pr = process flagData "--user_id 123"
     assertFlagValueEquals pr customHelp Nothing

testOptionalFlagMissingValue :: UnitTest
testOptionalFlagMissingValue = "An optional flag without value should report error" `unitTest`
  do let flagData = makeFlagData [f2d userId, f2d userLastName]
         pr = process flagData "--user_id 123 --user_last_name"
     assertError pr "Error with flag '--user_last_name': Flag value was not provided"
     assertSingleError pr

testOptionalFlagCorrectValue :: UnitTest
testOptionalFlagCorrectValue = "An optional flag with correct value should work correctly" `unitTest`
  do let flagData = makeFlagData [f2d userId, f2d userLastName]
         pr = process flagData "--user_id 123 --user_last_name batman"
     assertFlagValueEquals pr userLastName (Just "batman")
     assertFlagValueEquals pr userId 123

testOptionalFlagIncorrectValue :: UnitTest
testOptionalFlagIncorrectValue = "An optional flag with incorrect value should report error" `unitTest`
  do let flagData = makeFlagData [f2d userId, f2d customHelp]
         pr = process flagData "--user_id 123 --custom_help blah"
     assertError pr "Error with flag '--custom_help': Value 'blah' is not valid"
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

testRequiredIfNotRequired :: UnitTest
testRequiredIfNotRequired = "A requiredIf flag that returns false on the predicate should not be required" `unitTest`
  do let flagData = makeFlagData [f2d userId, f2d database]
         pr = process flagData "--user_id 1234"
     assertFlagValueEquals pr database Nothing
     assertFlagValueEquals pr userId 1234

testRequiredIfRequired :: UnitTest
testRequiredIfRequired = "A requiredIf flag that returns true on the predicate should be required" `unitTest`
  do let flagData = makeFlagData [f2d userId, f2d database]
         pr = process flagData "--user_id 4444"
     assertError pr "Error with flag '--database': Flag is required"
     assertSingleError pr

testRequiredIfRequiredButProvided :: UnitTest
testRequiredIfRequiredButProvided = "A requiredIf flag that returns true on the predicate but was provided should work correctly" `unitTest`
  do let flagData = makeFlagData [f2d userId, f2d database]
         pr = process flagData "--user_id 4444 --database mock"
     assertFlagValueEquals pr database (Just "mock")
     assertFlagValueEquals pr userId 4444

testRequiredIfRequiredButMissing :: UnitTest
testRequiredIfRequiredButMissing = "A requiredIf flag with missing value should report error " `unitTest`
  do let flagData = makeFlagData [f2d userId, f2d database]
         pr = process flagData "--user_id 4444 --database "
     assertError pr "Error with flag '--database': Flag value was not provided"
     assertSingleError pr

testGlobalValidationOccursAtTheEnd :: UnitTest
testGlobalValidationOccursAtTheEnd  = "Global validation should not execute if local validation failed" `unitTest`
  do let flagData = makeFlagData [f2d userId, f2d database]
         pr = process flagData ""
     assertError pr "Error with flag '--user_id': Flag is required"
     assertSingleError pr


testOperationEquals :: UnitTest
testOperationEquals  = "Equality operator should be parsed correctly" `unitTest`
  do let flagData = makeFlagData [f2d userId]
         pr = process flagData "--user_id = 123"
     assertFlagValueEquals pr userId 123

-- todo: Append operation not yet implemented
testOperationAppend :: UnitTest
testOperationAppend   = "Append operation should be parsed correctly" `unitTest`
  do let flagData = makeFlagData [f2d userId]
         pr = process flagData "--user_id 100 --user_id += 123"
     assertFlagValueEquals pr userId 123

testOrderOfFlags :: UnitTest
testOrderOfFlags = "Last flag should override value of previous" `unitTest`
  do let flagData = makeFlagData [f2d userId]
         pr = process flagData "--user_id 123 a b c --user_id 222"
     assertFlagValueEquals pr userId 222

testOrderOfArgs :: UnitTest
testOrderOfArgs = "Order of command line arguments must be preserved" `unitTest`
  do let flagData = makeFlagData [f2d userId]
         pr = process flagData "a b --user_id 123 c d --user_id 222 e f"
     assertArgsEquals pr ["a", "b", "c", "d", "e", "f"]

testFlagOperationWhitespace1 :: UnitTest
testFlagOperationWhitespace1 = "Whitespace between flag/value scenario 1" `unitTest`
  do let flagData = makeFlagData [f2d userId]
         pr = process flagData "--user_id = 123"
     assertFlagValueEquals pr userId 123

testFlagOperationWhitespace2 :: UnitTest
testFlagOperationWhitespace2 = "Whitespace between flag/value scenario 2" `unitTest`
  do let flagData = makeFlagData [f2d userId]
         pr = process flagData "--user_id= 123"
     assertFlagValueEquals pr userId 123

testFlagOperationWhitespace3 :: UnitTest
testFlagOperationWhitespace3 = "Whitespace between flag/value scenario 3" `unitTest`
  do let flagData = makeFlagData [f2d userId]
         pr = process flagData "--user_id =123"
     assertFlagValueEquals pr userId 123

testFlagOperationWhitespace4 :: UnitTest
testFlagOperationWhitespace4 = "Whitespace between flag/value scenario 4" `unitTest`
  do let flagData = makeFlagData [f2d userId]
         pr = process flagData "--user_id=123"
     assertFlagValueEquals pr userId 123

testFlagOperationWhitespace5 :: UnitTest
testFlagOperationWhitespace5 = "Whitespace between flag/value scenario 5" `unitTest`
  do let flagData = makeFlagData [f2d userId]
         pr = process flagData "--user_id\t\n=123"
     assertFlagValueEquals pr userId 123

testFlagOperationWhitespace6 :: UnitTest
testFlagOperationWhitespace6 = "Whitespace between flag/value scenario 6" `unitTest`
  do let flagData = makeFlagData [f2d userId]
         pr = process flagData "--user_id=\t\n123"
     assertFlagValueEquals pr userId 123

testFlagOperationWhitespace7 :: UnitTest
testFlagOperationWhitespace7 = "Whitespace between flag/value scenario 7" `unitTest`
  do let flagData = makeFlagData [f2d userId]
         pr = process flagData "--user_id\t\n=\t\n123"
     assertFlagValueEquals pr userId 123

testHelpKeywordReserved :: UnitTest
testHelpKeywordReserved = "The keyword 'help' is reserved" `unitTest`
  do let flagData = makeFlagData [f2d userId, f2d help]
         pr = process flagData "--user_id 123"
     assertError pr "Error with flag '--help': The name is a reserved word and can not be used"
     assertSingleError pr

testHelpKeywordReservedOnAlias :: UnitTest
testHelpKeywordReservedOnAlias = "The keyword 'help' is reserved on alias" `unitTest`
  do let flagData = makeFlagData [f2d userId, f2d help]
         pr = process flagData "--user_id 123"
     assertError pr "Error with flag '--help': The name is a reserved word and can not be used"
     assertSingleError pr

testUsingfFileKeywordReserved :: UnitTest
testUsingfFileKeywordReserved  = "The keyword 'usingFile' is reserved" `unitTest`
  do let flagData = makeFlagData [f2d userId, f2d usingFileFlag]
         pr = process flagData "--user_id 123"
     assertError pr "Error with flag '--usingFile': The name is a reserved word and can not be used"
     assertSingleError pr

testEmptyFlagFollowedByFlag :: UnitTest
testEmptyFlagFollowedByFlag = "A flag followed by another flag should have a value of 'empty'" `unitTest`
  do let flagData = makeFlagData [f2d userLastName]
         pr = process flagData "--user_last_name --user_last_name"
     assertError pr "Error with flag '--user_last_name': Flag value was not provided"
     assertSingleError pr

testFlagAlias :: UnitTest
testFlagAlias = "Valid flag alias should be parsed correctly" `unitTest`
  do let flagData = makeFlagData [f2d userId]
         pr = process flagData "--uid 123"
     assertFlagValueEquals pr userId 123

testFlagAliasIncorrectValue :: UnitTest
testFlagAliasIncorrectValue = "Valid flag alias with invalid value should report an error" `unitTest`
  do let flagData = makeFlagData [f2d userId]
         pr = process flagData "--uid blah"
     assertError pr "Error with flag '--user_id': Value 'blah' is not valid"
     assertSingleError pr

{-# ANN module "HLint: ignore Reduce duplication" #-}
{-# ANN module "HLint: ignore Use camelCase" #-}

