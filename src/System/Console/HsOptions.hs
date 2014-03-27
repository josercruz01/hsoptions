module System.Console.HsOptions(
    make,
    get,

    intParser,
    stringParser,
    boolParser,

    boolFlag,

    flagToData,
    combine,
    process,
    process',
    processMain,
    defaultDisplayHelp,

    isOptional,
    emptyValueIs,
    defaultIs,
    aliasIs,
    parser,
    maybeParser,
    requiredIf,

    Flag(..),
    FlagData,
    FlagError(..),
    FlagResults,
    ProcessResults,
    ArgsResults,
    FlagDataConf(..),
    FlagConf(..)
) where

import Data.List
import Data.Maybe
import Text.Read(readMaybe)
import System.Environment
import System.Console.HsOptions.Parser
import Control.Exception
import qualified System.Console.GetOpt as Opt
import qualified Data.Map as Map

data Flag a = Flag String String [FlagConf a]
data FlagError = FlagNonFatalError String | FlagFatalError String deriving (Show)
type FlagData = (Map.Map String FlagDataAtom, FlagAliasMap)
type FlagDataAtom = (String, [FlagDataConf])
type FlagResults = (Map.Map String FlagArgument)
type ArgsResults = [String]
type FlagAliasMap = (Map.Map String String)
type ParseResults  = (FlagResults, ArgsResults)
type ProcessResults  = (FlagResults, ArgsResults)
type PipelineFunction = (FlagData -> FlagResults -> ([FlagError], FlagResults))
data FlagArgument = FlagMissing String 
                  | FlagValueMissing String
                  | FlagValue String String
                  deriving (Show)

data ValidationResult = ValidationError FlagError
                      | ValidationSuccess 


data FlagConf a = 
    FlagConf_DefaultIs a
  | FlagConf_RequiredIf (FlagResults -> Bool)
  | FlagConf_Parser (FlagArgument -> Maybe a)
  | FlagConf_EmptyValueIs a
  | FlagConf_Alias [String]

data FlagDataConf = 
    FlagDataConf_HasDefault 
  | FlagDataConf_RequiredIf  (FlagResults -> Bool)
  | FlagDataConf_Validator (FlagArgument -> Bool) 
  | FlagDataConf_HasEmptyValue
  | FlagDataConf_Alias [String]

flagErrorMessage :: String -> String -> String
flagErrorMessage name msg = "Error with flag '--" ++ name ++ "': " ++ msg

usingFileKeyword :: [String]
usingFileKeyword = ["usingFile"]

helpKeyword :: [String]
helpKeyword = ["help", "h"]

reservedWords :: [String]
reservedWords = usingFileKeyword ++ helpKeyword

isOptional :: FlagConf (Maybe a)
isOptional = requiredIf (const False)

emptyValueIs :: a -> FlagConf a
emptyValueIs = FlagConf_EmptyValueIs

defaultIs :: a -> FlagConf a
defaultIs = FlagConf_DefaultIs 

aliasIs :: [String] -> FlagConf a
aliasIs = FlagConf_Alias 

requiredIf :: (FlagResults -> Bool) -> FlagConf (Maybe a)
requiredIf = FlagConf_RequiredIf 

parser :: (FlagArgument -> Maybe a) -> FlagConf a
parser = FlagConf_Parser 

maybeParser :: (FlagArgument -> Maybe a) -> FlagConf (Maybe a)
maybeParser p =  FlagConf_Parser p'
  where p' = maybeParserWrapper p

maybeParserWrapper :: (FlagArgument -> Maybe a) -> FlagArgument -> Maybe (Maybe a)
maybeParserWrapper _p (FlagMissing _) = Just Nothing
maybeParserWrapper _p (FlagValueMissing _) = Just Nothing
maybeParserWrapper p flagValue = case p flagValue of
                                    Nothing -> Nothing
                                    justSomething -> Just justSomething

emptyFlagResults :: Map.Map String FlagArgument
emptyFlagResults = Map.empty

emptyArgsResults :: [String]
emptyArgsResults = []


get :: FlagResults -> Flag a ->  a
get result (Flag name _ flagconf) = fromJust $ runParser flagconf argValue
    where argValue = fromMaybe (error ("Error while trying to get flag value for '" ++ name ++ "'." ++
                                       " Perhaps this flag was not added to the flagData array"))
                               (Map.lookup name result )

flagDefault :: [FlagConf a] -> Maybe a
flagDefault fc = listToMaybe [ x | (FlagConf_DefaultIs x) <- fc]

flagAlias :: [FlagConf a] -> [String]
flagAlias fc = concat [ x | (FlagConf_Alias x) <- fc]

flagDAlias :: [FlagDataConf] -> [String]
flagDAlias fc = concat [ x | (FlagDataConf_Alias x) <- fc]

flagEmptyValue :: [FlagConf a] -> Maybe a
flagEmptyValue fc = listToMaybe [ x | (FlagConf_EmptyValueIs x) <- fc]

runRealParser :: [FlagConf a] -> FlagArgument -> Maybe a
runRealParser flagconf = p
  where p = head [x | (FlagConf_Parser x) <- flagconf]

runParser :: [FlagConf a] -> FlagArgument -> Maybe a
runParser fc arg@(FlagMissing _) = case flagDefault fc of
    Nothing -> runRealParser fc arg
    Just val -> Just val
runParser fc arg@(FlagValueMissing _) = case flagEmptyValue fc of
    Nothing -> runRealParser fc arg
    Just val -> Just val
runParser fc arg = runRealParser fc arg

combine :: [FlagData] -> FlagData
combine = foldl auxUnion (Map.empty, Map.empty)
  where auxUnion (m1, a1) (m2, a2) = case findDuplicate (m1, a1) (m2, a2) of
                             [] -> (m1 `Map.union` m2, a1 `Map.union` a2)
                             flags -> error ("Duplicate flag names: " ++
                                             "The following flags names are duplicated in the code " ++ 
                                             show flags)

        findDuplicate (m1, a1) (m2, a2) = (Map.keys m1 ++ Map.keys a1) `intersect`
                                          (Map.keys m2 ++ Map.keys a2) 
                                        

flagToData :: Flag a -> FlagData
flagToData (Flag name help flagConf) = (Map.fromList [(name, (help, flagDataConf))], Map.fromList alias)
  where alias = map (\s -> (s, name)) (flagAlias flagConf)
        flagDataConf = map aux flagConf
        aux (FlagConf_DefaultIs _) = FlagDataConf_HasDefault
        aux (FlagConf_RequiredIf predicate) = FlagDataConf_RequiredIf predicate
        aux (FlagConf_EmptyValueIs _) = FlagDataConf_HasEmptyValue
        aux (FlagConf_Parser p) = FlagDataConf_Validator (isJust . p)
        aux (FlagConf_Alias as) = FlagDataConf_Alias as

fromAliasMaybe :: FlagAliasMap -> String -> String
fromAliasMaybe alias s = fromMaybe s (Map.lookup s alias)

executeOp :: ParseResults -> (String, OperationToken, FlagValueToken) -> FlagResults
executeOp _state (name, _op, FlagValueTokenEmpty) = Map.fromList [(name, FlagValueMissing name)]
executeOp _state (name, _op, FlagValueToken value) = Map.fromList [(name, FlagValue name value)]

parseToken :: (ParseResults, Token) -> ParseResults
parseToken (state, FlagToken name op value) = (executeOp state (name, op, value), [])
parseToken (_, ArgToken arg) = (emptyFlagResults, [arg])

parseArgs :: [Token] -> ParseResults -> ParseResults
parseArgs [] state = state
parseArgs (tok:toks) state = parseArgs toks (state `mergeParseResults` res)
  where res = parseToken (state, tok)

mergeParseResults :: ParseResults -> ParseResults -> ParseResults
mergeParseResults (fr1, args1) (fr2, args2) = (fr2 `Map.union` fr1, args1 ++ args2)

type TokenizeResult = Either [FlagError] [Token]

concatToks :: TokenizeResult -> TokenizeResult -> TokenizeResult
concatToks (Left errs) (Left errs2) = Left (errs ++ errs2)
concatToks (Left errs) _ = Left errs
concatToks _ (Left errs) = Left errs
concatToks (Right toks1) (Right toks2) = Right (toks1 ++ toks2)

parseConfigFile :: String -> IO TokenizeResult
parseConfigFile filename = 
  do fileResult <- try $ readFile filename :: IO (Either SomeException String)
     case fileResult of 
         Left except -> return (Left [FlagFatalError ("Error on '" ++ filename  ++ "': " ++ show except)])
         Right content -> tokenize content

isUsingConfFlag :: Token -> Maybe String
isUsingConfFlag (FlagToken "usingFile" _ (FlagValueToken filename)) = Just filename
isUsingConfFlag _ = Nothing

includeConfig :: [Token] -> IO TokenizeResult
includeConfig [] = return (Right [])
includeConfig (t:ts) = case isUsingConfFlag t of
                          Nothing -> do restToks <- includeConfig ts
                                        return (Right [t] `concatToks` restToks)
                          Just conf -> do confToks <- parseConfigFile conf
                                          restToks <- includeConfig ts
                                          return (confToks `concatToks` restToks)

tokenize :: String -> IO TokenizeResult
tokenize input = includeConfig (parseInput input)

process :: FlagData -> [String] -> IO (Either [FlagError] ProcessResults)
process fd args = do result <- tokenize (unwords args)
                     case result of
                      Left errs -> return (Left errs)
                      Right toks -> return (process' fd toks)

process' :: FlagData -> [Token] -> Either [FlagError] ProcessResults
process' fd toks = case pipeline [validateReservedWords,
                                  addMissingFlags, 
                                  validateUnknownFlags, 
                                  validateFlagParsers]
                                 [validateGlobal]
                                 fd
                                 flagResults of
                      ([],res) -> Right (res, argsResults)
                      (errs,_) -> Left errs
  where toks' = preprocess fd toks
        (flagResults, argsResults) = parseArgs toks' (emptyFlagResults, emptyArgsResults)

preprocess :: FlagData -> [Token] -> [Token]
preprocess (_fd, aliasMap) = map changeAlias
  where changeAlias (FlagToken name op value) = FlagToken (fromAliasMaybe aliasMap name) op value
        changeAlias t = t

anyArgIsHelp :: [String] -> Bool
anyArgIsHelp args = any (`elem` args) helpFlags
  where helpFlags = concat [["--" ++ x, "-" ++ x] | x <- helpKeyword]

processMain :: String -> -- program description
               FlagData -> -- flags
               (ProcessResults -> IO ()) ->  -- success function. run program
               ([FlagError] -> IO ()) -> -- failure function. show errors
               (String -> [(String, [String], String)] -> IO ()) -> -- help display function
               IO () 
processMain desc fd success failure displayHelp = 
    do args <- getArgs 
       if anyArgIsHelp args 
          then displayHelp desc (getFlagHelp fd)
          else do result <- process fd args 
                  case result of 
                      Left errs -> failure errs
                      Right res -> success res

hasFatalError :: [FlagError] -> Bool
hasFatalError errs = not . null $ [x | x@(FlagFatalError _) <- errs]

pipeline :: [PipelineFunction] -> [PipelineFunction] -> PipelineFunction
pipeline validation1 validation2 fd fr = 
  case pipeline' validation1 fd fr of
    ([], res) -> pipeline' validation2 fd res
    errs -> errs

pipeline' :: [PipelineFunction] -> PipelineFunction
pipeline' [] _fd fr = ([], fr)
pipeline' (v:vs) fd fr = case v fd fr of 
    ([], fr') -> pipeline' vs fd fr' 
    (errs, fr') -> if hasFatalError errs 
                   then (errs, fr')
                   else let (errs'', fr'') = pipeline' vs fd fr' in 
                        (errs ++ errs'', fr'')

validateReservedWords :: PipelineFunction
validateReservedWords (fd, aliasMap) fr = case reservedWords `intersect` codeFlags of
                                  [] -> ([], fr)
                                  names -> (map reservedWordsError names, fr)
  where codeFlags = Map.keys fd ++ Map.keys aliasMap
        reservedWordsError name = FlagFatalError $ flagErrorMessage name 
                                                   "The name is a reserved word and can not be used"

ifSomething :: Maybe a -> (a -> Bool) -> Bool
ifSomething Nothing _ = True
ifSomething (Just a) p = p a

addMissingFlags :: PipelineFunction
addMissingFlags (fd, aliasMap)  fr = ([], fr `Map.union` Map.fromList flags)
  where inputFlags = Map.keys fr
        codeFlags = Map.keys fd
        missingFlags = [x | x <- codeFlags, 
                            x `notElem` inputFlags,
                            ifSomething (Map.lookup x aliasMap) (`notElem` inputFlags)]
        flags = map (\ name -> (name, FlagMissing name)) missingFlags

validateUnknownFlags :: PipelineFunction
validateUnknownFlags (fd, aliasMap) fr = (errors, fr)
  where inputFlags = Map.keys fr
        codeFlags = Map.keys fd ++ Map.keys aliasMap
        missingFlags = inputFlags \\ codeFlags
        errors = map flagUnkownError missingFlags
        flagUnkownError name = FlagFatalError $ flagErrorMessage name "Unkown flag is not defined in the code"

validateFlagParsers :: PipelineFunction
validateFlagParsers (fd, _aliasMap) fr = (mapMaybe aux (Map.toList fd), fr)
  where aux :: (String, FlagDataAtom) -> Maybe FlagError
        aux (name, (_, flagDataConf)) = case checkValidator flagDataConf value of
                                           ValidationError err -> Just err
                                           _ -> Nothing 
          where value = fromJust (Map.lookup name fr)

validateGlobal :: PipelineFunction
validateGlobal (fd, _aliasMap) fr = (mapMaybe aux (Map.toList fd), fr)
  where aux :: (String, FlagDataAtom) -> Maybe FlagError
        aux (name, (_, flagDataConf)) = case requiredIfValidator flagDataConf fr value of
                                             ValidationError err -> Just err
                                             _ -> Nothing 
          where value = fromJust (Map.lookup name fr)

requiredIfValidator :: [FlagDataConf] -> FlagResults -> FlagArgument -> ValidationResult
requiredIfValidator fdc fr (FlagMissing name) 
  | flagDIsRequiredIf fdc fr = validationError name "Flag is required"
  | otherwise = ValidationSuccess
requiredIfValidator _fdc _fr _flagArg = ValidationSuccess

make :: (String, String, [FlagConf a]) -> Flag a
make (name, help, flagConf) = if hasParser 
                              then Flag name help flagConf
                              else error (flagErrorMessage name "Flag parser was not provided")
  where hasParser = not . null $ [x | (FlagConf_Parser x) <- flagConf]

defaultDisplayHelp :: String -> [(String, [String], String)] -> IO ()
defaultDisplayHelp desc flags = putStrLn $ Opt.usageInfo desc (map getOptDescr flags)
  where getShortName = foldl (\ (s, l) current -> if length current == 1 
                                                  then (s ++ [head current], l)
                                                  else (s, l ++ [current])) ([], [])

        getOptDescr (name, alias, help) = Opt.Option short (name:long) (Opt.NoArg "") help
          where (short, long) = getShortName alias
        

getFlagHelp :: FlagData -> [(String, [String], String)]
getFlagHelp (fd, _aliasMap) = let flags = Map.toList fd in
                 map (\ (name, (help, flagDataConf)) -> (name, flagDAlias flagDataConf, help)) flags ++ 
                     [("help", ["h"] ,"show this help")]

flagDIsOptional :: [FlagDataConf] -> Bool
flagDIsOptional fdc = not . null $ [True | (FlagDataConf_RequiredIf _) <- fdc] 

flagDIsRequiredIf :: [FlagDataConf] -> FlagResults -> Bool
flagDIsRequiredIf fdc fr = case maybePredicate of 
                              Nothing -> False
                              Just p -> p fr
   where maybePredicate = listToMaybe [ predicate | (FlagDataConf_RequiredIf predicate) <- fdc] 

flagDHasDefault :: [FlagDataConf] -> Bool
flagDHasDefault fdc = not . null $ [ x | x@FlagDataConf_HasDefault <- fdc] 

flagDHasEmptyValue :: [FlagDataConf] -> Bool
flagDHasEmptyValue fdc = not . null $ [ x | x@FlagDataConf_HasEmptyValue <- fdc] 

runDValidator :: [FlagDataConf] -> FlagArgument -> Bool
runDValidator fdc = validator 
  where validator = head [x | (FlagDataConf_Validator x) <- fdc]

validationError :: String -> String -> ValidationResult
validationError name s = ValidationError $ FlagNonFatalError (flagErrorMessage name s)

checkValidator :: [FlagDataConf] -> FlagArgument -> ValidationResult
checkValidator fdc (FlagMissing name) 
  | flagDIsOptional fdc = ValidationSuccess
  | flagDHasDefault fdc = ValidationSuccess
  | otherwise = validationError name "Flag is required"
checkValidator fdc (FlagValueMissing name) 
  | flagDHasEmptyValue fdc  = ValidationSuccess
  | otherwise = validationError name "Flag value was not provided"
checkValidator fdc flagArgument@(FlagValue name value) 
  | runDValidator fdc flagArgument = ValidationSuccess 
  | otherwise = validationError name $ "Value '" ++ value ++ "' is not valid"

{- Flag parsers -}
intParser :: FlagArgument -> Maybe Int
intParser (FlagMissing _) = Nothing
intParser (FlagValueMissing _) = Nothing
intParser (FlagValue _ value) = readMaybe value

stringParser :: FlagArgument -> Maybe String
stringParser (FlagMissing _) = Nothing
stringParser (FlagValueMissing _) = Nothing
stringParser (FlagValue _ value) = Just value

boolParser :: FlagArgument -> Maybe Bool
boolParser (FlagMissing _) = Just False
boolParser (FlagValueMissing _) = Just True
boolParser (FlagValue _ value) = readMaybe value

boolFlag :: [FlagConf Bool]
boolFlag = [parser boolParser,
            defaultIs False,
            emptyValueIs True]

{-# ANN module "HLint: ignore Use camelCase" #-}
