module System.Console.HsOptionsTestHelpers 
where

import Test.HUnit
import Control.Monad
import Control.Exception
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

assertError :: TestProcessResult -> String -> Assertion
assertError (TestProcessSuccess _) errorMessage = 
  assertFailure $ "assertError failed. expected '" ++ errorMessage ++ "' but zero errors occurred"
assertError (TestProcessError errs) errorMessage = 
  let nfErrs = [er | (HSO.FlagNonFatalError er) <- errs] ++ [er | (HSO.FlagFatalError er) <- errs]in
  when ( errorMessage `notElem` nfErrs)
    (assertFailure $ "assertError failed. expected '" ++ 
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

assertFlagValueEquals (TestProcessSuccess (results, _args)) flag expected = assertEqual "" expected value 
  where value = HSO.get results flag

assertArgsEquals :: TestProcessResult -> [String] -> Assertion
assertArgsEquals (TestProcessError errs) _args = 
  assertFailure ("assertFlagValueEquals failed. expected no errors when getting args value" ++
                " but errors found:\n" ++
                "** errors that where found:\n" ++ 
                errorsToString errs)
assertArgsEquals (TestProcessSuccess (_results, args)) expected = assertEqual "" expected args 


assertFlagDataException :: HSO.FlagData -> String -> Assertion
assertFlagDataException fd msg = do result <- try (evaluate fd) :: IO (Either SomeException HSO.FlagData)
                                    case result of
                                        Left err -> assertEqual "" msg (show err)
                                        Right _ -> fail "Expected exception but no exception occurred"

process :: HSO.FlagData -> String -> IO TestProcessResult
process fd input = do result <- HSO.process fd (words input)
                      case result of 
                          Left errs -> return (TestProcessError errs)
                          Right r -> return (TestProcessSuccess r)

makeFlagData :: [HSO.FlagData] -> HSO.FlagData
makeFlagData = HSO.combine


makeConfFile :: [String] -> String
makeConfFile = unlines

validate :: HSO.GlobalRule -> HSO.FlagData
validate = HSO.validate

