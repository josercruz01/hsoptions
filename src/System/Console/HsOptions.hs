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
import Data.List
import Data.Maybe
import Text.Read(readMaybe)
import qualified Data.Map as Map

data Flag a = Flag String String (FlagArgument -> Either FlagError a)
data FlagError = FlagNonFatalError String | FlagFatalError String deriving (Show)
type FlagData = Map.Map String FlagDataAtom
type FlagDataAtom = (String, FlagArgument -> Maybe FlagError)
type FlagResults = Map.Map String FlagArgument
type ArgsResults = [String]
type ProcessResults  = (FlagResults, ArgsResults)
type PipelineFunction = (FlagData -> FlagResults -> ([FlagError], FlagResults))
data FlagArgument = FlagMissing String 
                  | FlagValueMissing String
                  | FlagValue String String

emptyFlagResults :: Map.Map String FlagArgument
emptyFlagResults = Map.empty

emptyArgsResults :: [String]
emptyArgsResults = []

areDigits :: String -> Bool
areDigits "" = False
areDigits s = all isDigit s

takeRight :: Either a b -> b
takeRight (Right b) = b
takeRight _ = error "Error trying to takeRight of a Left"

get :: FlagResults -> Flag a ->  a
get result (Flag name _ parser) = takeRight $ parser argValue
    where argValue = fromMaybe (error ("Error while trying to get flag value for '" ++ name ++ "'." ++
                                      " Perhaps this flag was not added to the flagData array"))
                               (Map.lookup name result )

combine :: [FlagData] -> FlagData
combine = foldl Map.union Map.empty

flagToData :: Flag a -> FlagData
flagToData (Flag name help parser) = Map.fromList [(name, (help, hasAnyError parser))]

hasAnyError :: (FlagArgument -> Either FlagError a) -> FlagArgument -> Maybe FlagError
hasAnyError parser s = case parser s of
  Left err -> Just err
  _ -> Nothing

isFlagName :: String -> Bool
isFlagName "-" = False
isFlagName "--" = False
isFlagName name 
  | take 2 name == "--" = True
  | take 1 name == "-" && (not . areDigits) (drop 1 name) = True
  | otherwise = False

getFlagName :: String -> String
getFlagName name 
  | take 2 name == "--" = drop 2 name
  | otherwise = drop 1 name

makeFlagResults :: (String, FlagArgument) -> ProcessResults 
makeFlagResults flagArg = (Map.fromList [flagArg], emptyArgsResults)

processFlag ::  String -> [String] -> (ProcessResults, [String])
processFlag name [] = (makeFlagResults (name, FlagValueMissing name), [])
processFlag name (arg2:args)
    | isFlagName arg2 = (makeFlagResults (name, FlagValueMissing name), arg2:args)
    | otherwise = (makeFlagResults (name, FlagValue name arg2), args)

parseArg ::  String -> [String] -> (ProcessResults, [String])
parseArg arg args = 
  if isFlagName arg
    then processFlag (getFlagName arg) args
    else ((emptyFlagResults, [arg]), args) 

parseArgs ::  [String] -> ProcessResults -> ProcessResults
parseArgs arguments res = case arguments of
    [] -> res
    arg:args -> let (res',args') = parseArg arg args in 
                parseArgs args' (merge res res')
  where merge (pr1, args1) (pr2, args2) = ( pr1 `Map.union` pr2, args1 ++ args2)
        

process :: FlagData -> [String] -> Either [FlagError] ProcessResults
process flagData args = case pipeline [addMissingFlags,
                                       validateUnknownFlags,
                                       validateLocal,
                                       validateFlagParsers, 
                                       validateGlobal]
                             flagData
                             flagResults of
    ([],res) -> Right (res, argsResults)
    (errs,_) -> Left errs
  where (flagResults, argsResults) = parseArgs args (emptyFlagResults, emptyArgsResults)

pipeline :: [PipelineFunction] -> PipelineFunction
pipeline [] _fd fr = ([], fr)
pipeline (v:vs) fd fr = case v fd fr of 
    ([], fr') -> pipeline vs fd fr' 
    (errs, fr') -> (errs, fr')

validateLocal :: PipelineFunction
validateLocal _fd fr = ([], fr)

addMissingFlags :: PipelineFunction
addMissingFlags fd fr = ([], fr `Map.union` Map.fromList flags)
  where inputFlags = Map.keys fr
        codeFlags = Map.keys fd
        missingFlags = codeFlags \\ inputFlags 
        flags = map (\ name -> (name, FlagMissing name)) missingFlags

validateUnknownFlags :: PipelineFunction
validateUnknownFlags fd fr = (errors, fr)
  where inputFlags = Map.keys fr
        codeFlags = Map.keys fd
        missingFlags = inputFlags \\ codeFlags
        errors = map flagUnkownError missingFlags
        flagUnkownError name = FlagNonFatalError $ "Error with flag --" ++
                               name ++
                               ": Unkown flag is not defined in the code"

validateFlagParsers :: PipelineFunction
validateFlagParsers fd fr = (mapMaybe aux (Map.toList fd), fr)
  where aux :: (String, FlagDataAtom) -> Maybe FlagError
        aux (name, (_, validator)) = case Map.lookup name fr of 
                                      Nothing -> Nothing
                                      Just val -> validator val

validateGlobal :: PipelineFunction
validateGlobal _fd fr = ([], fr)

make :: (String, String, FlagArgument -> Either FlagError a) -> Flag a
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

mkError :: String -> String -> Either FlagError b
mkError name s = Left $ FlagNonFatalError ("Error with flag '--" ++name ++ "': " ++ s)

required :: (String -> Maybe a) -> FlagArgument -> Either FlagError a
required _parser (FlagMissing name) = mkError name "Flag is required"
required _parser (FlagValueMissing name) = mkError name "Flag value was not provided"
required parser (FlagValue name value) = case parser value of 
  Nothing -> mkError name $ "Value '" ++ value ++ "' is not valid"
  Just val -> Right val

optional :: (String -> Maybe a) -> FlagArgument -> Either FlagError (Maybe a)
optional _parser (FlagMissing _name) = Right Nothing
optional _parser (FlagValueMissing name) = mkError name "Flag value was not provided"
optional parser (FlagValue name value) = case parser value of 
  Nothing -> mkError name $ "Value '" ++ value ++ "' is not valid"
  val -> Right val

{- Flag parsers -}
intFlag :: String -> Maybe Int
intFlag = readMaybe 

stringFlag :: String -> Maybe String
stringFlag = Just 

boolFlag :: String -> Maybe Bool
boolFlag = readMaybe

