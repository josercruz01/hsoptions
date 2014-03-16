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
    flagOptional,
    maybeFlag,
    Flag(..),
    FlagData,
    FlagError(..),
    FlagResults,
    ProcessResults,
    ArgsResults,
    FlagConstraint(..)
) where

import Data.List
import Data.Maybe
import Text.Read(readMaybe)
import qualified Data.Map as Map

data Flag a = Flag String String (String -> Maybe a)
data FlagError = FlagNonFatalError String | FlagFatalError String deriving (Show)
type FlagData = (Map.Map String FlagDataAtom, [FlagConstraint])
type FlagDataAtom = (String, String -> Bool)
type FlagResults = (Map.Map String FlagArgument)
type FlagLookup = (FlagResults, [FlagConstraint])
type ArgsResults = [String]
type ParseResults  = (FlagResults, ArgsResults)
type ProcessResults  = (FlagLookup, ArgsResults)
type PipelineFunction = (FlagData -> FlagResults -> ([FlagError], FlagResults))
data FlagArgument = FlagMissing String 
                  | FlagValueMissing String
                  | FlagValue String String


data FlagConstraint = FlagOptional String deriving (Show)

emptyFlagResults :: Map.Map String FlagArgument
emptyFlagResults = Map.empty

emptyArgsResults :: [String]
emptyArgsResults = []

isInteger :: String -> Bool
isInteger s = case reads s :: [(Integer, String)] of
                [(_, "")] -> True
                _         -> False

isDouble :: String -> Bool
isDouble s = case reads s :: [(Double, String)] of
                [(_, "")] -> True
                _         -> False

isNumeric :: String -> Bool
isNumeric s = isInteger s || isDouble s

takeRight :: Either a b -> b
takeRight (Right b) = b
takeRight _ = error "Error trying to takeRight of a Left"

get :: FlagLookup -> Flag a ->  a
get (result, constraints) (Flag name _ parser) = fromJust $ takeRight (case r of 
                                                          Left err -> error $ "Error is: " ++ show err
                                                          Right r -> Right r)
    where argValue = fromMaybe (error ("Error while trying to get flag value for '" ++ name ++ "'." ++
                                       " Perhaps this flag was not added to the flagData array"))
                               (Map.lookup name result )

          r = required parser constraints argValue

combine :: [FlagData] -> [FlagConstraint] -> FlagData
combine fd constraints = foldl merge (Map.empty, constraints) fd
  where merge (fd1, const1) (fd2, const2) = (fd1 `Map.union` fd2, const1 ++ const2) 

flagToData :: Flag a -> FlagData
flagToData (Flag name help parser) = (Map.fromList [(name, (help, isJust . parser))], [])

isFlagName :: String -> Bool
isFlagName "-" = False
isFlagName "--" = False
isFlagName name 
  | take 2 name == "--" = True
  | take 1 name == "-" && (not . isNumeric) (drop 1 name) = True
  | otherwise = False

getFlagName :: String -> String
getFlagName name 
  | take 2 name == "--" = drop 2 name
  | otherwise = drop 1 name

makeFlagResults :: (String, FlagArgument) -> ParseResults 
makeFlagResults flagArg = (Map.fromList [flagArg], emptyArgsResults)

processFlag ::  String -> [String] -> (ParseResults, [String])
processFlag name [] = (makeFlagResults (name, FlagValueMissing name), [])
processFlag name (arg2:args)
    | isFlagName arg2 = (makeFlagResults (name, FlagValueMissing name), arg2:args)
    | otherwise = (makeFlagResults (name, FlagValue name arg2), args)

parseArg ::  String -> [String] -> (ParseResults, [String])
parseArg arg args = 
  if isFlagName arg
    then processFlag (getFlagName arg) args
    else ((emptyFlagResults, [arg]), args) 

parseArgs ::  [String] -> ParseResults -> ParseResults
parseArgs arguments res = case arguments of
    [] -> res
    arg:args -> let (res',args') = parseArg arg args in 
                parseArgs args' (merge res res')
  where merge (pr1, args1) (pr2, args2) = ( pr1 `Map.union` pr2, args1 ++ args2)
        

process :: FlagData -> [String] -> Either [FlagError] ProcessResults
process flagData@(_fd, constraints) args = case pipeline [addMissingFlags,
                                       validateUnknownFlags,
                                       validateLocal,
                                       validateFlagParsers, 
                                       validateGlobal]
                             flagData
                             flagResults of
    ([],res) -> Right ((res, constraints) , argsResults)
    (errs,_) -> Left errs
  where (flagResults, argsResults) = parseArgs args (emptyFlagResults, emptyArgsResults)

pipeline :: [PipelineFunction] -> PipelineFunction
pipeline [] _fd fr = ([], fr)
pipeline (v:vs) fd fr = case v fd fr of 
    ([], fr') -> pipeline vs fd fr' 
    (errs, fr') -> (errs, fr')

validateLocal :: PipelineFunction
validateLocal (_fd, _const) fr = ([], fr)

addMissingFlags :: PipelineFunction
addMissingFlags (fd, _const) fr = ([], fr `Map.union` Map.fromList flags)
  where inputFlags = Map.keys fr
        codeFlags = Map.keys fd
        missingFlags = codeFlags \\ inputFlags 
        flags = map (\ name -> (name, FlagMissing name)) missingFlags

validateUnknownFlags :: PipelineFunction
validateUnknownFlags (fd, _const) fr = (errors, fr)
  where inputFlags = Map.keys fr
        codeFlags = Map.keys fd
        missingFlags = inputFlags \\ codeFlags
        errors = map flagUnkownError missingFlags
        flagUnkownError name = FlagNonFatalError $ "Error with flag --" ++
                               name ++
                               ": Unkown flag is not defined in the code"

validateFlagParsers :: PipelineFunction
validateFlagParsers (fd, constraint) fr = (mapMaybe aux (Map.toList fd), fr)
  where aux :: (String, FlagDataAtom) -> Maybe FlagError
        aux (name, (_, validator)) = case Map.lookup name fr of 
                                      Nothing -> Nothing
                                      Just val -> case requiredAux validator constraint val of
                                                   ValueParserError err -> Just err
                                                   _ -> Nothing
                                                 

validateGlobal :: PipelineFunction
validateGlobal (_fd, _const) fr = ([], fr)

make :: (String, String, String -> Maybe a) -> Flag a
make (name, help, parser) = Flag name help parser

showHelp :: String -> FlagData -> IO ()
showHelp desc (flagData, _const) = do 
  putStrLn desc
  putStrLn ""
  putStrLn "Usage:"
  putStrLn ""
  let flags = Map.toList flagData
  mapM_ aux flags
  where aux (name, (help, _)) = putStrLn $ name ++ ":\t\t" ++ help

mkErrorAux :: String -> String -> ValueParser
mkErrorAux name s = ValueParserError $ FlagNonFatalError ("Error with flag '--" ++name ++ "': " ++ s)

flagIsOptional :: String -> [FlagConstraint] -> Bool
flagIsOptional name constraints = name `elem` optionalFlags
  where optionalFlags = [n | (FlagOptional n) <- constraints]


data ValueParser = ValueParserSkip
                |  ValueParserError FlagError
                |  ValueParserNoError String
requiredAux :: (String -> Bool) -> [FlagConstraint] -> FlagArgument -> ValueParser
requiredAux _parser constraints (FlagMissing name) = if flagIsOptional name constraints
  then ValueParserSkip
  else mkErrorAux name "Flag is required"
requiredAux _parser _constraints (FlagValueMissing name) = mkErrorAux name "Flag value was not provided"
requiredAux parser _constraints (FlagValue name value) = if not (parser value)
  then mkErrorAux name $ "Value '" ++ value ++ "' is not valid"
  else ValueParserNoError value

required :: (String -> Maybe a) -> [FlagConstraint] -> FlagArgument -> Either FlagError (Maybe a)
required parser constraint flagArg = case requiredAux (isJust . parser) constraint flagArg of
    ValueParserSkip -> Right Nothing
    ValueParserNoError value -> Right $ parser value
    ValueParserError err -> Left err

maybeFlag :: Read a => (String -> Maybe a) -> String -> Maybe (Maybe a)
maybeFlag _parser s = Just (readMaybe s)

flagOptional :: Flag (Maybe a) -> FlagConstraint
flagOptional (Flag name _ _) = FlagOptional name

{- Flag parsers -}
intFlag :: String -> Maybe Int
intFlag = readMaybe 

stringFlag :: String -> Maybe String
stringFlag = Just 

boolFlag :: String -> Maybe Bool
boolFlag = readMaybe

