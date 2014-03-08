module System.Console.HsOptionsTestHelpers 
where

import Test.HUnit
import Control.Monad
import qualified System.Console.HsOptions as HSO

f2d :: HSO.Flag a -> HSO.FlagData
f2d = HSO.flagToData

data TestProcessResult = 
    TestProcessError [HSO.FlagError] 
  | TestProcessSuccess HSO.ProcessResults

errorsToString :: [HSO.FlagError] -> String
errorsToString  [] = ""
errorsToString  (er:errs) = aux er ++ errorsToString errs
  where aux (HSO.FlagNonFatalError erMessage) = " * '" ++ erMessage ++ "'\n"
        aux (HSO.FlagFatalError erMessage) = " * '" ++erMessage ++ "'\n"

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

assertFlagValueEquals (TestProcessSuccess (results, _)) flag expected = assertEqual "" value expected
  where value = HSO.get results flag

process :: HSO.FlagData -> String -> TestProcessResult
process fd input = case HSO.process fd args of
    Left errs -> TestProcessError errs
    Right result -> TestProcessSuccess result
  where args = words input

makeFlagData :: [HSO.FlagData] -> HSO.FlagData
makeFlagData = HSO.combine
