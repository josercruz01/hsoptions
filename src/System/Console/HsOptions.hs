module System.Console.HsOptions(
    make,
    get,
    intFlag,
    stringFlag,
    boolFlag,
    flagToData,
    combine,
    process,
    showHelp,
    required,
    optional,
    Flag(..),
    FlagData,
    FlagError(..),
    FlagResults,
    ProcessResults,
    ArgsResults
) where

import Data.Char
import Data.Maybe
import Text.Read(readMaybe)
import qualified Data.Map as Map

data Flag a = Flag String String (Maybe String -> Either FlagError a)
data FlagError = FlagNonFatalError String | FlagFatalError String deriving (Show)
type FlagDataAtom = (String, Maybe String -> Maybe FlagError)
type FlagData = Map.Map String FlagDataAtom


type FlagResults = Map.Map String String
type ArgsResults = [String]
type ProcessResults  = (FlagResults, ArgsResults)

emptyFlagResults :: Map.Map String String
emptyFlagResults = Map.empty

emptyArgsResults :: [String]
emptyArgsResults = []

emptyProcessResults :: (FlagResults, ArgsResults)
emptyProcessResults = (emptyFlagResults, emptyArgsResults)

mkProcessResults :: FlagResults -> ArgsResults -> ProcessResults
mkProcessResults fr ar = (fr, ar)

mkFlagResults :: FlagData -> String -> String -> FlagResults 
mkFlagResults _flagData name value = Map.fromList [(realName, value)]
  where realName = name -- todo(jrc2316): map from alias to real name

noErrors :: [FlagError]
noErrors = []

areDigits :: String -> Bool
areDigits "" = False
areDigits s = all isDigit s

takeRight :: Either a b -> b
takeRight (Right b) = b
takeRight _ = error "Error trying to takeRight of a Left"

get ::  FlagResults -> Flag a ->  a
get result (Flag name _ parser) = takeRight $ parser argValue
    where argValue = Map.lookup name result

combine :: [FlagData] -> FlagData
combine = foldl Map.union Map.empty

combineProcessResults :: (ProcessResults, ProcessResults) -> ProcessResults
combineProcessResults ((pr1, args1), (pr2, args2)) = ( pr1 `Map.union` pr2, args1 ++ args2)

flagToData :: Flag a -> FlagData
flagToData (Flag name help parser) = Map.fromList [(name, (help, hasAnyError parser))]

hasAnyError :: (Maybe String -> Either FlagError a) -> Maybe String -> Maybe FlagError
hasAnyError parser s = case parser s of
  Left err -> Just err
  _ -> Nothing

isFlagName :: String -> Bool
isFlagName name 
  | take 2 name == "--" = True
  | take 1 name == "-" && (not . areDigits) (drop 1 name) = True
  | otherwise = False

getFlagName :: String -> String
getFlagName name 
  | take 2 name == "--" = drop 2 name
  | otherwise = drop 1 name

getFlagDataAtom :: FlagData -> String -> Maybe FlagDataAtom
getFlagDataAtom flagData name = Map.lookup name flagData

processFlag :: FlagData -> String -> [String] -> ([FlagError], ProcessResults, [String])
processFlag _ "" args = 
  ([FlagNonFatalError "Incorrect systax. Found '--' or '-' with no name afterwards"],
   emptyProcessResults, 
   args)
processFlag flagData name [] = processFlag flagData name [""]
processFlag flagData name (arg2:args)
  | isFlagName arg2 = processFlag flagData name ("":arg2:args)
  | otherwise = case getFlagDataAtom flagData name of
    Nothing ->
      ([FlagNonFatalError ("Passed in flag '--" ++ name ++"' was not defined in the code")],
       emptyProcessResults,
       args)
    Just (_, validator) -> case validator (Just arg2) of
      Nothing -> ([], mkProcessResults (mkFlagResults flagData name arg2) [], args)
      Just err -> ([prependError ("Error with flag '--" ++name ++ "': ") err],
                   emptyProcessResults,
                   args)

processArg :: FlagData -> String -> [String] -> ([FlagError], ProcessResults, [String])
processArg flagData arg args = 
  if isFlagName arg
    then processFlag flagData (getFlagName arg) args
    else (noErrors, mkProcessResults emptyFlagResults [arg], args)

fatalError :: FlagError -> Bool
fatalError (FlagFatalError _err) = True
fatalError _ = False

processAux :: FlagData -> [String] -> ([FlagError], ProcessResults) -> ([FlagError], ProcessResults)
processAux flagData arguments (errs,res) = case arguments of
  [] -> (errs,res)
  arg:args ->
    let (errs',res',args') = processArg flagData arg args in
    if any fatalError errs'
    then (errs ++ errs', combineProcessResults (res, res'))
    else processAux flagData args' (errs ++ errs', combineProcessResults (res, res'))

process :: FlagData -> [String] -> Either [FlagError] ProcessResults
process flagData args = case processAux flagData args ([], emptyProcessResults) of
    ([],res) ->
      case validateGlobal flagData res of
        [] -> Right res
        errs -> Left errs
    (errs,_) -> Left errs

validateGlobal :: FlagData -> ProcessResults -> [FlagError]
validateGlobal flagData (flagResults, _) = missingFlagErrors
  where missingFlagErrors = mapMaybe checkMissingFlag (Map.toList flagData)
        checkMissingFlag :: (String, FlagDataAtom) -> Maybe FlagError
        checkMissingFlag (name, (_, validator)) = case validator value of
          Nothing -> Nothing
          Just er -> Just $ prependError ("Error with flag '--" ++name ++ "': ") er
          where value = Map.lookup name flagResults

make :: (String, String, Maybe String -> Either FlagError a) -> Flag a
make (name, help, parser) = Flag name help parser

showHelp :: String -> FlagData -> IO ()
showHelp desc flagData = do 
  putStrLn desc
  putStrLn ""
  putStrLn "Usage:"
  putStrLn ""
  let flags = Map.toList flagData
  mapM_ aux flags
  where aux (name, (help, _)) = putStrLn $ name ++ ":\t\t" ++ help

mkError :: String -> Either FlagError b
mkError s = Left $ FlagNonFatalError s

prependError :: String -> FlagError -> FlagError
prependError msg (FlagNonFatalError err) = FlagNonFatalError (msg ++ err)
prependError msg (FlagFatalError err) = FlagFatalError (msg ++ err)

required :: (String -> Maybe a) -> Maybe String -> Either FlagError a
required _parser Nothing = mkError "Flag is required"
required parser (Just s) = case parser s of 
  Nothing -> mkError $ "Value '" ++ s ++ "' is not valid"
  Just val -> Right val

optional :: (String -> Maybe a) -> Maybe String -> Either FlagError (Maybe a)
optional _parser Nothing = Right Nothing
optional parser (Just s) = case parser s of 
  Nothing -> mkError $ "Value '" ++ s ++ "' is not valid"
  val -> Right val

{- Flag parsers -}
intFlag :: String -> Maybe Int
intFlag = readMaybe 

stringFlag :: String -> Maybe String
stringFlag = Just 

boolFlag :: String -> Maybe Bool
boolFlag = readMaybe

