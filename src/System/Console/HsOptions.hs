module System.Console.HsOptions(
    make,
    get,

    intParser,
    stringParser,
    boolParser,

    boolFlag,

    flagToData,
    combine,
    validate,
    operation,
    assign,
    append,
    append',
    process,
    process',
    processMain,
    defaultDisplayHelp,

    isOptional,
    emptyValueIs,
    defaultIs,
    defaultIf,
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
    FlagConf(..),
    GlobalRule
) where

import Data.List
import Data.Maybe
import Text.Read(readMaybe)
import System.Environment
import System.Console.HsOptions.Parser
import Control.Exception
import Text.Regex.Posix
import System.Directory
import qualified System.Console.GetOpt as Opt
import qualified Data.Map as Map

data Flag a = Flag String String [FlagConf a]
data FlagError = FlagNonFatalError String | FlagFatalError String 
type FlagData = (Map.Map String FlagDataAtom, FlagAliasMap, [GlobalRule])
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

type GlobalRule = FlagResults -> Maybe String

data ValidationResult = ValidationError FlagError
                      | ValidationSuccess 

data FlagConf a = 
    FlagConf_DefaultIf a (FlagResults -> Bool)
  | FlagConf_RequiredIf (FlagResults -> Bool)
  | FlagConf_Parser (FlagArgument -> Maybe a)
  | FlagConf_EmptyValueIs a
  | FlagConf_Alias [String]
  | FlagConf_DefaultOperation OperationToken

data FlagDataConf = 
    FlagDataConf_HasDefault  (FlagResults -> Bool)
  | FlagDataConf_RequiredIf  (FlagResults -> Bool)
  | FlagDataConf_Validator (FlagArgument -> Bool) 
  | FlagDataConf_HasEmptyValue
  | FlagDataConf_Alias [String]
  | FlagDataConf_DefaultOperation OperationToken

instance Show FlagError where
  show (FlagFatalError err) = err
  show (FlagNonFatalError err) = err

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

operation :: OperationToken -> FlagConf a
operation = FlagConf_DefaultOperation 

append :: OperationToken
append = OperationTokenAppend

append' :: OperationToken
append' = OperationTokenAppend'

assign :: OperationToken
assign = OperationTokenAssign

emptyValueIs :: a -> FlagConf a
emptyValueIs = FlagConf_EmptyValueIs

defaultIs :: a -> FlagConf a
defaultIs a = FlagConf_DefaultIf a (const True)

defaultIf :: a -> (FlagResults -> Bool) -> FlagConf a
defaultIf = FlagConf_DefaultIf

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
get result (Flag name _ flagconf) = fromJust $ runParser result flagconf argValue
    where argValue = fromMaybe (error ("Error while trying to get flag value for '" ++ name ++ "'." ++
                                       " Perhaps this flag was not added to the flagData array"))
                               (Map.lookup name result )

flagDefault :: FlagResults -> [FlagConf a] -> Maybe a
flagDefault fr fc = case listToMaybe [ (x, p) | (FlagConf_DefaultIf x p) <- fc] of
                      Nothing -> Nothing
                      Just (x, p) -> if p fr then Just x else Nothing

flagAlias :: [FlagConf a] -> [String]
flagAlias fc = concat [ x | (FlagConf_Alias x) <- fc]

flagDAlias :: [FlagDataConf] -> [String]
flagDAlias fc = concat [ x | (FlagDataConf_Alias x) <- fc]

flagDDefaultOperation :: [FlagDataConf] -> OperationToken
flagDDefaultOperation fc = case [ x | (FlagDataConf_DefaultOperation x) <- fc] of
                              [] -> OperationTokenAssign
                              res -> head res

flagEmptyValue :: [FlagConf a] -> Maybe a
flagEmptyValue fc = listToMaybe [ x | (FlagConf_EmptyValueIs x) <- fc]

runRealParser :: [FlagConf a] -> FlagArgument -> Maybe a
runRealParser flagconf = p
  where p = head [x | (FlagConf_Parser x) <- flagconf]

runParser :: FlagResults -> [FlagConf a] -> FlagArgument -> Maybe a
runParser fr fc arg@(FlagMissing _) = case flagDefault fr fc of
    Nothing -> runRealParser fc arg
    Just val -> Just val
runParser _fr fc arg@(FlagValueMissing _) = case flagEmptyValue fc of
    Nothing -> runRealParser fc arg
    Just val -> Just val
runParser _fr fc arg = runRealParser fc arg

combine :: [FlagData] -> FlagData
combine = foldl auxUnion (Map.empty, Map.empty, [])
  where auxUnion (m1, a1, gr1) (m2, a2, gr2) = case findDuplicate (m1, a1) (m2, a2) of
                             [] -> (m1 `Map.union` m2, a1 `Map.union` a2, gr1 ++ gr2)
                             flags -> error ("Duplicate flag names: " ++
                                             "The following flags names are duplicated in the code " ++ 
                                             show flags)

        findDuplicate (m1, a1) (m2, a2) = (Map.keys m1 ++ Map.keys a1) `intersect`
                                                (Map.keys m2 ++ Map.keys a2) 

validate :: GlobalRule -> FlagData
validate rule = (Map.empty, Map.empty, [rule])

flagToData :: Flag a -> FlagData
flagToData flag@(Flag name help flagConf) = case invalidFlag flag of
                                                Nothing -> result
                                                Just err -> error err
  where result = (Map.singleton name (help, flagDataConf), Map.fromList alias, [])
        alias = map (\s -> (s, name)) (flagAlias flagConf)
        flagDataConf = map aux flagConf
        aux (FlagConf_DefaultIf _ p) = FlagDataConf_HasDefault p
        aux (FlagConf_RequiredIf predicate) = FlagDataConf_RequiredIf predicate
        aux (FlagConf_EmptyValueIs _) = FlagDataConf_HasEmptyValue
        aux (FlagConf_Parser p) = FlagDataConf_Validator (isJust . p)
        aux (FlagConf_Alias as) = FlagDataConf_Alias as
        aux (FlagConf_DefaultOperation op) = FlagDataConf_DefaultOperation op

        invalidFlag (Flag n _ fc) = if null invalidFlags
                                       then Nothing
                                       else Just ("Error: The following flags names are invalid " ++ 
                                                  show invalidFlags)
            where aliasList = flagAlias fc
                  allNames = n:aliasList
                  invalidFlags = [x | x <- allNames, invalidFlagName x]
                  invalidFlagName :: String -> Bool
                  invalidFlagName s = not (s =~ "^[a-zA-Z][a-zA-Z0-9\\-_]*$" :: Bool)

fromAliasMaybe :: FlagAliasMap -> String -> String
fromAliasMaybe alias s = fromMaybe s (Map.lookup s alias)

executeOp :: ParseResults -> (String, OperationToken, FlagValueToken) -> FlagResults

executeOp _st (name, OperationTokenAssign, FlagValueToken value) = result
  where result = Map.singleton name (FlagValue name value)

executeOp _st (name, OperationTokenAssign, FlagValueTokenEmpty) = result
  where result = Map.singleton name (FlagValueMissing name)


executeOp res (name, OperationTokenAppend, FlagValueToken value) = result
  where result = executeOp res (name, OperationTokenAppend', FlagValueToken (" " ++ value))

executeOp res (name, OperationTokenAppend, FlagValueTokenEmpty) = result
  where result = executeOp res (name, OperationTokenAppend', FlagValueTokenEmpty)

executeOp (fr, _) (name, OperationTokenAppend', FlagValueToken value) = result
  where result = Map.singleton name (FlagValue name (previous ++ value))
        previous = case Map.lookup name fr of
                      Just (FlagValue _ v) -> v
                      _ -> ""

executeOp (fr, _) (name, OperationTokenAppend', FlagValueTokenEmpty) = result
  where result = Map.singleton name value
        value = case Map.lookup name fr of
                  Just fv@(FlagValue _ _) -> fv
                  _ -> FlagValueMissing name

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
concatToks (Right toks1) (Right toks2) = Right $ toks1 ++ toks2

parseConfigFile :: [String] -> FlagData -> String -> IO (TokenizeResult, [String])
parseConfigFile past fd filename = 
  do fileResult <- try $ readFile filename :: IO (Either SomeException String)
     case fileResult of 
         Left except -> return (Left [FlagFatalError ("Error on '" ++ filename  ++ "': " ++ show except)], past)
         Right content -> tokenize past fd (removeComments content)

  where removeComments "" = ""
        removeComments (c:cs) = if c == '#'
                                then dropLine cs
                                else c:removeComments cs

        dropLine "" = ""
        dropLine ('\n':cs) = removeComments cs
        dropLine (_:cs) =  dropLine cs

isUsingConfFlag :: Token -> IO (Maybe String)
isUsingConfFlag (FlagToken "usingFile" _ (FlagValueToken filename)) = do path <- canonicalizePath filename
                                                                         return $ Just path
isUsingConfFlag _ = return Nothing

includeConfig :: [String] -> FlagData -> [Token] -> IO (TokenizeResult, [String])
includeConfig past _fd [] = return (Right [], past)
includeConfig past fd (t:ts) = do isUsingConf <- isUsingConfFlag t 
                                  case isUsingConf of
                                    Nothing -> do (restToks, past') <- includeConfig past fd ts
                                                  return (Right [t] `concatToks` restToks, past')
                                    Just conf -> let past' = past ++ [conf] in
                                                 if conf `elem` past
                                                 then reportCircularDependency past'
                                                 else do (confToks, _) <- parseConfigFile past' fd conf
                                                         (restToks, _) <- includeConfig past fd ts
                                                         return (confToks `concatToks` restToks, past)

reportCircularDependency :: [String] -> IO (TokenizeResult, [String])
reportCircularDependency past = return (Left [FlagFatalError 
      $ "Error while parsing conf file: Circular includes on files\n" ++ aux past], past)
  where aux [] = ""
        aux [x] = "  " ++ x
        aux (x:xs) = "  " ++ x ++ " ->\n" ++ aux xs

tokenize :: [String] -> FlagData -> String -> IO (TokenizeResult, [String])
tokenize past flagData@(fd, _, _) input = includeConfig past flagData (parseInput defaultOp input)
  where defaultOp = mkDefaultOp $ Map.toList fd

mkDefaultOp :: [(String, FlagDataAtom)] -> DefaultOp
mkDefaultOp [] = Map.empty
mkDefaultOp ((name, (_, flagDataConf)):rest) = Map.singleton name defaultOp' `Map.union` nextOps
  where nextOps = mkDefaultOp rest
        defaultOp' = flagDDefaultOperation flagDataConf

process :: FlagData -> [String] -> IO (Either [FlagError] ProcessResults)
process fd args = do (result, _) <- tokenize [] fd (unwords args)
                     case result of
                      Left errs -> return (Left errs)
                      Right toks -> return (process' fd toks)

process' :: FlagData -> [Token] -> Either [FlagError] ProcessResults
process' fd toks = case pipeline [validateReservedWords,
                                  addMissingFlags, 
                                  validateUnknownFlags, 
                                  validateFlagParsers]
                                 [validateRequiredIf,
                                  validateDependentDefault,
                                  validateGlobalRules]
                                 fd
                                 flagResults of
                      ([],res) -> Right (res, argsResults)
                      (errs,_) -> Left errs
  where toks' = preprocess fd toks
        (flagResults, argsResults) = parseArgs toks' (emptyFlagResults, emptyArgsResults)

preprocess :: FlagData -> [Token] -> [Token]
preprocess (_fd, aliasMap, _gr) = map changeAlias
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
    do args <- getQuotedArgs 
       if anyArgIsHelp args 
          then displayHelp desc (getFlagHelp fd)
          else do result <- process fd args 
                  case result of 
                      Left errs -> failure errs
                      Right res -> success res

getQuotedArgs  :: IO [String]
getQuotedArgs = do args <- getArgs
                   return (map quote' args)
  where quote' s | length (words s) > 1 = "\"" ++ s ++ "\""
                 | otherwise = s

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
validateReservedWords (fd, aliasMap, _gr) fr = case reservedWords `intersect` codeFlags of
                                  [] -> ([], fr)
                                  names -> (map reservedWordsError names, fr)
  where codeFlags = Map.keys fd ++ Map.keys aliasMap
        reservedWordsError name = FlagFatalError $ flagErrorMessage name 
                                                   "The name is a reserved word and can not be used"

ifSomething :: Maybe a -> (a -> Bool) -> Bool
ifSomething Nothing _ = True
ifSomething (Just a) p = p a

addMissingFlags :: PipelineFunction
addMissingFlags (fd, aliasMap, _gr)  fr = ([], fr `Map.union` Map.fromList flags)
  where inputFlags = Map.keys fr
        codeFlags = Map.keys fd
        missingFlags = [x | x <- codeFlags, 
                            x `notElem` inputFlags,
                            ifSomething (Map.lookup x aliasMap) (`notElem` inputFlags)]
        flags = map (\ name -> (name, FlagMissing name)) missingFlags

validateUnknownFlags :: PipelineFunction
validateUnknownFlags (fd, aliasMap, _gr) fr = (errors, fr)
  where inputFlags = Map.keys fr
        codeFlags = Map.keys fd ++ Map.keys aliasMap
        missingFlags = inputFlags \\ codeFlags
        errors = map flagUnkownError missingFlags
        flagUnkownError name = FlagFatalError $ flagErrorMessage name "Unkown flag is not defined in the code"

validateFlagParsers :: PipelineFunction
validateFlagParsers (fd, _aliasMap, _gr) fr = (mapMaybe aux (Map.toList fd), fr)
  where aux :: (String, FlagDataAtom) -> Maybe FlagError
        aux (name, (_, flagDataConf)) = case checkValidator flagDataConf value of
                                           ValidationError err -> Just err
                                           _ -> Nothing 
          where value = fromJust (Map.lookup name fr)

validateRequiredIf :: PipelineFunction
validateRequiredIf (fd, _aliasMap, _gr) fr = (mapMaybe aux (Map.toList fd), fr)
  where aux :: (String, FlagDataAtom) -> Maybe FlagError
        aux (name, (_, flagDataConf)) = case requiredIfValidator flagDataConf fr value of
                                             ValidationError err -> Just err
                                             _ -> Nothing
          where value = fromJust (Map.lookup name fr)

validateDependentDefault :: PipelineFunction
validateDependentDefault (fd, _aliasMap, _gr) fr = (mapMaybe aux (Map.toList fd), fr)
  where aux :: (String, FlagDataAtom) -> Maybe FlagError
        aux (name, (_, flagDataConf)) = case defaultIfValidator flagDataConf fr value of
                                             ValidationError err -> Just err
                                             _ -> Nothing
          where value = fromJust (Map.lookup name fr)

validateGlobalRules :: PipelineFunction
validateGlobalRules (_fd, _aliasMap, gr) fr = (flagErrs, fr)
  where errs = mapMaybe (\ r -> r fr) gr 
        flagErrs = map FlagNonFatalError errs

requiredIfValidator :: [FlagDataConf] -> FlagResults -> FlagArgument -> ValidationResult
requiredIfValidator fdc fr (FlagMissing name) 
  | flagDIsRequiredIf fdc fr = validationError name "Flag is required"
  | otherwise = ValidationSuccess
requiredIfValidator _fdc _fr _flagArg = ValidationSuccess

defaultIfValidator :: [FlagDataConf] -> FlagResults -> FlagArgument -> ValidationResult
defaultIfValidator fdc fr (FlagMissing name) 
  | flagDHasDefault fdc = if flagDGetDefaultIf fdc fr 
                          then ValidationSuccess
                          else validationError name "Flag is required"
  | otherwise = ValidationSuccess
defaultIfValidator _fdc _fr _flagArg = ValidationSuccess

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
getFlagHelp (fd, _aliasMap, _gr) = let flags = Map.toList fd in
                 map (\ (name, (help, flagDataConf)) -> (name, flagDAlias flagDataConf, help)) flags ++ 
                     [("usingFile", [] ,"read flags from configuration file")] ++
                     [("help", ["h"] ,"show this help")]

flagDIsOptional :: [FlagDataConf] -> Bool
flagDIsOptional fdc = not . null $ [True | (FlagDataConf_RequiredIf _) <- fdc] 

flagDIsRequiredIf :: [FlagDataConf] -> FlagResults -> Bool
flagDIsRequiredIf fdc fr = case maybePredicate of 
                              Nothing -> False
                              Just p -> p fr
   where maybePredicate = listToMaybe [ predicate | (FlagDataConf_RequiredIf predicate) <- fdc] 

flagDGetDefaultIf :: [FlagDataConf] -> FlagResults -> Bool
flagDGetDefaultIf fdc fr = case maybePredicate of 
                              Nothing -> False
                              Just p -> p fr
   where maybePredicate = listToMaybe [ predicate | (FlagDataConf_HasDefault predicate) <- fdc] 

flagDHasDefault :: [FlagDataConf] -> Bool
flagDHasDefault fdc = not . null $ [ x | x@(FlagDataConf_HasDefault _) <- fdc] 

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
