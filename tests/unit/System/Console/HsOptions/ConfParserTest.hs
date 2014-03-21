module System.Console.HsOptions.ConfParserTest where

import UnitTestHelper
import System.Console.HsOptionsTestHelpers

tests :: [UnitTest]
tests = [
    testSingleFlag,
    testTwoFlags,
    testEmptyLines,
    testInvalidSintax1,
    testInvalidSintax2,
    testWhitespaceValid
  ]

testSingleFlag :: UnitTest
testSingleFlag  = "Single flag should be parsed correctly" `unitTest`
  do let confFile = makeConfFile ["user_id = 123"]
         result = parse confFile 
     assertConfArgsEquals result ["user_id", "123"]

testTwoFlags :: UnitTest
testTwoFlags  = "Two flags should be parsed correctly" `unitTest`
  do let confFile = makeConfFile ["user_id = 123", "user_name = batman"]
         result = parse confFile 
     assertConfArgsEquals result ["user_id", "123", 
                                  "user_name", "batman"]

testEmptyLines :: UnitTest
testEmptyLines  = "Empty lines should be skipped" `unitTest`
  do let confFile = makeConfFile ["user_id = 123", 
                                  "\t", 
                                  "   ", 
                                  "user_name = batman"]
         result = parse confFile 
     assertConfArgsEquals result ["user_id", "123", 
                                  "user_name", "batman"]

testInvalidSintax1 :: UnitTest
testInvalidSintax1  = "Flag with no operation and no value should report error" `unitTest`
  do let confFile = makeConfFile ["user_id = 123", "user_name"]
         result = parse confFile 
     assertNonFatalConfError result "Syntax error near 'user_name'"

testInvalidSintax2 :: UnitTest
testInvalidSintax2  = "Flag with no value should report error" `unitTest`
  do let confFile = makeConfFile ["user_id=123", "user_name="]
         result = parse confFile 
     assertNonFatalConfError result "Syntax error near 'user_name='"

testWhitespaceValid :: UnitTest
testWhitespaceValid  = "Whitespace should be ignored correctly" `unitTest`
  do let confFile = makeConfFile ["\tuser_id\t     =     123 \t\n"]
         result = parse confFile 
     assertConfArgsEquals result ["user_id", "123"]
