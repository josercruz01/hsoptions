module System.Console.HsOptionsTestHelpers 
where

import Test.HUnit
import Control.Monad
import qualified System.Console.HsOptions as HSO
import qualified System.Console.HsOptions.ConfParser as Parser

f2d :: HSO.Flag a -> HSO.FlagData
f2d = HSO.flagToData

data TestProcessResult = 
    TestProcessError [HSO.FlagError] 
  | TestProcessSuccess HSO.ProcessResults

data TestConfParseResult = 
    TestConfParseResultError [Parser.ConfParserError] 
  | TestConfParseResultSuccess [String]

errorsToString :: [HSO.FlagError] -> String
errorsToString  [] = ""
errorsToString  (er:errs) = aux er ++ errorsToString errs
  where aux (HSO.FlagNonFatalError erMessage) = " * '" ++ erMessage ++ "'\n"
        aux (HSO.FlagFatalError erMessage) = " * '" ++erMessage ++ "'\n"

confErrorsToString :: [Parser.ConfParserError] -> String
confErrorsToString errs =  unlines (map (\e-> " * '" ++ show e ++ "'") errs)

assertNonFatalError :: TestProcessResult -> String -> Assertion
assertNonFatalError (TestProcessSuccess _) errorMessage = 
  assertFailure $ "assertNonFatalError failed. expected '" ++ errorMessage ++ "' but zero errors occurred"
assertNonFatalError (TestProcessError errs) errorMessage = 
  let nfErrs = [er | (HSO.FlagNonFatalError er) <- errs] in
  when ( errorMessage `notElem` nfErrs)
    (assertFailure $ "assertNonFatalError failed. expected '" ++ 
                       errorMessage ++ 
                       "' but error not found.\n" ++
                       "** other errors that where found:\n" ++ 
                       errorsToString errs)

assertSingleError :: TestProcessResult -> Assertion
assertSingleError (TestProcessSuccess _) = 
  assertFailure "assertSingleError failed. expected single error but zero errors occurred"
assertSingleError (TestProcessError errs) = let count = length errs in
  when (count /= 1) (assertFailure $ "assertSingleError failed. expected single error but "++ 
                                    show count ++ " errors occurred")


assertFlagValueEquals :: (Eq a, Show a) => TestProcessResult -> HSO.Flag a -> a -> Assertion
assertFlagValueEquals (TestProcessError errs) _flag _value = 
  assertFailure ("assertFlagValueEquals failed. expected no errors when getting flag value" ++
                " but errors found:\n" ++
                "** errors that where found:\n" ++ 
                errorsToString errs)

assertFlagValueEquals (TestProcessSuccess (results, _args)) flag expected = assertEqual "" value expected
  where value = HSO.get results flag

assertArgsEquals :: TestProcessResult -> [String] -> Assertion
assertArgsEquals (TestProcessError errs) _args = 
  assertFailure ("assertFlagValueEquals failed. expected no errors when getting args value" ++
                " but errors found:\n" ++
                "** errors that where found:\n" ++ 
                errorsToString errs)
assertArgsEquals (TestProcessSuccess (_results, args)) expected = assertEqual "" args expected

process :: HSO.FlagData -> String -> TestProcessResult
process fd input = case HSO.process fd args of
    Left errs -> TestProcessError errs
    Right result -> TestProcessSuccess result
  where args = words input

makeFlagData :: [HSO.FlagData] -> HSO.FlagData
makeFlagData = HSO.combine


makeConfFile :: [String] -> String
makeConfFile = unlines

parse :: String -> TestConfParseResult 
parse s = case Parser.parseFromString "test.conf" s of 
  Left errs -> TestConfParseResultError errs
  Right result -> TestConfParseResultSuccess result

assertConfArgsEquals :: TestConfParseResult -> [String] -> Assertion
assertConfArgsEquals (TestConfParseResultError errs) _args = 
  assertFailure  ("assertConfArgsEquals failed. expected no errors when comparing results" ++
                  " but errors found:\n" ++
                  "** errors that where found:\n" ++ 
                  confErrorsToString errs)
assertConfArgsEquals  (TestConfParseResultSuccess results) expected = assertEqual "" expected results 

assertNonFatalConfError :: TestConfParseResult  -> String -> Assertion
assertNonFatalConfError  (TestConfParseResultSuccess _) errorMessage = 
  assertFailure $ "assertNonFatalConfError failed. expected '" ++ errorMessage ++ "' but zero errors occurred"
assertNonFatalConfError   (TestConfParseResultError errs) errorMessage = 
  let nfErrs = [show x | x@(Parser.ConfParserNonFatalError _ _) <- errs] in
  let expectedError = "Error on 'test.conf': " ++ errorMessage in
  when ( expectedError `notElem` nfErrs)
    (assertFailure $ "assertNonFatalConfError failed. expected '" ++ 
                       expectedError ++ 
                       "' but error not found.\n" ++
                       "** other errors that where found:\n" ++ 
                       confErrorsToString errs)

assertFatalConfError :: TestConfParseResult  -> String -> Assertion
assertFatalConfError  (TestConfParseResultSuccess _) errorMessage = 
  assertFailure $ "assertFatalConfError failed. expected '" ++ errorMessage ++ "' but zero errors occurred"
assertFatalConfError   (TestConfParseResultError errs) errorMessage = 
  let nfErrs = [show x | x@(Parser.ConfParserFatalError _ _) <- errs] in
  when ( errorMessage `notElem` nfErrs)
    (assertFailure $ "assertFatalConfError failed. expected '" ++ 
                       errorMessage ++ 
                       "' but error not found.\n" ++
                       "** other errors that where found:\n" ++ 
                       confErrorsToString errs)

assertSingleConfError :: TestConfParseResult  -> Assertion
assertSingleConfError  (TestConfParseResultSuccess _) = 
  assertFailure "assertSingleConfError failed. expected single error but zero errors occurred"
assertSingleConfError   (TestConfParseResultError errs) = let count = length errs in
  when (count /= 1) (assertFailure $ "assertSingleConfError failed. expected single error but "++ 
                                    show count ++ " errors occurred\n" ++ 
                                    "** errors  found:\n" ++ 
                                    confErrorsToString errs)
