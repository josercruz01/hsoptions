{- |
Module      :  System.Console.HsOptions
Description :  Command line flag parser for Haskell
Copyright   :  (c) Jose Raymundo Cruz (jose.r.cruz01@gmail.com)
License     :  Apache-2.0

Maintainer  :  Jose Raymundo Cruz (jose.r.cruz01@gmail.com)
Stability   :  stable
Portability :  portable

@HsOptions@ library supports command line flag parsing.

Flags are declared in the code by using the 'make' method, which
takes  the @name@, @help text@ and @flag type@ (Int, Bool, String, etc) as
arguments.

The flags are parsed from the command line stream of from a file
if the @--usingFile <filename>@ flag is sent to the program.

Flags can be customized by calling configuration methods, such as
@defaultIs@ or @aliasIs@, that change how the flag behaves, how it
is parsed and validated.

The @processMain@ method needs to be called at the beginning of the @main@
method. This method takes as arguments the @program description@, a
list of all the declared flags and three callbacks (@success@, @failure@
and @display help@). If there is any kind of validation error @failure@ is
called with the list of errors. If the @--help@ flag was sent by the user
@display help@ is called (a default implementation of this function is
provided in the library, this is the @defaultDisplayHelp@ method).
Otherwise if there is no problems the @success@ method is called.

Basically @success@ becomes the 'real' main function. It takes as argument
a tuple (@FlagResults@, @ArgsResults@). @FlagResults@ is a data structure
that can be used to query flags by using the @get@ method. @ArgsResults@ is
just an array of @String@ containing the remaining not-flag arguments.

A simple example (more in
<https://github.com/josercruz01/hsoptions/tree/master/examples>)

> import System.Console.HsOptions
>
> userName = make ( "user_name",
>                 , "the user name of the app",
>                 , [ parser stringParser,
>                   , aliasIs ["u"]
>                   ]
>                 )
> userAge = make ("age", "the age of the user", [parser intParser])
>
> flagData = combine [flagToData userName, flagToData userAge]
>
> main :: IO ()
> main = processMain "Simple example for HsOptions."
>                    flagData
>                    success
>                    failure
>                    defaultDisplayHelp
>
> success :: ProcessResults -> IO ()
> success (flags, args) = do let nextAge = (flags `get` userAge) + 5
>                            putStrLn ("Hello " ++ flags `get` userName)
>                            putStrLn ("In 5 years you will be " ++
>                                      show nextAge ++
>                                      " years old!")
>
> failure :: [FlagError] -> IO ()
> failure errs = do putStrLn "Some errors occurred:"
>                   mapM_ print errs

At @processMain@ each the input stream is validated against the declared
flags. In the @success@ function you can be sure that all required flags
where verified to exist, all flag types are correct and all validation
was executed.
-}
module System.Console.HsOptions(
    -- * Definition of flags
    make,

    -- * Query flag values
    get,

    -- * Process flags
    processMain,
    process,
    process',
    flagToData,
    combine,

    -- * Flag types and parsers
    parser,
    maybeParser,
    intParser,
    floatParser,
    doubleParser,
    charParser,
    stringParser,
    boolParser,
    arrayParser,
    boolFlag,

    -- * Flag customization
    isOptional,
    emptyValueIs,
    defaultIs,
    defaultIf,
    aliasIs,
    requiredIf,

    -- * Global validation
    validate,

    -- * Flag operations
    operation,
    assign,
    append,
    append',

    -- * Default functions implementation
    defaultDisplayHelp,
    defaultDisplayErrors,

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

import Control.Exception
import Control.Monad
import Data.List
import Data.Maybe
import System.Console.HsOptions.Parser
import System.Directory
import System.Environment
import Text.Read(readMaybe)
import Text.Regex
import Text.Regex.Posix

import qualified Data.Map as Map
import qualified System.Console.GetOpt as Opt

data Flag a = Flag String String [FlagConf a]

data FlagError = FlagNonFatalError String
               | FlagFatalError String

type FlagData = (FlagDataMap, FlagAliasMap, [GlobalRule])
type FlagDataMap = Map.Map String FlagDatum
type FlagDatum = (String, [FlagDataConf])
type FlagAliasMap = (Map.Map String String)

type FlagResults = (Map.Map String FlagArgument)
data FlagArgument = FlagMissing String
                  | FlagValueMissing String
                  | FlagValue String String
                  deriving (Show)

type ArgsResults = [String]
type ParseResults  = (FlagResults, ArgsResults)
type ProcessResults  = (FlagResults, ArgsResults)
type PipelineFunction = (FlagData, FlagResults) -> ([FlagError], FlagResults)
type GlobalRule = FlagResults -> Maybe String

type TokenizeResult = Either [FlagError] [Token]

data ValidationResult =
    ValidationError FlagError
  | ValidationSuccess

data FlagConf a =
    FlagConf_DefaultIf a (FlagResults -> Bool)
  | FlagConf_RequiredIf (FlagResults -> Bool)
  | FlagConf_Parser (FlagArgument -> Maybe a)
  | FlagConf_EmptyValueIs a
  | FlagConf_Alias [String]
  | FlagConf_DefaultOperation OperationToken

data FlagDataConf =
    FlagDataConf_HasDefault (FlagResults -> Bool)
  | FlagDataConf_RequiredIf (FlagResults -> Bool)
  | FlagDataConf_Validator (FlagArgument -> Bool)
  | FlagDataConf_HasEmptyValue
  | FlagDataConf_Alias [String]
  | FlagDataConf_DefaultOperation OperationToken

instance Show FlagError where
  show (FlagFatalError err) = err
  show (FlagNonFatalError err) = err

flagError :: String -> String -> String
flagError name msg = "Error with flag '--" ++ name ++ "': " ++ msg

usingFileKw :: (String, [String])
usingFileKw = ("usingFile", [])

helpKw :: (String, [String])
helpKw = ("help", ["h"])

inheritKw :: String
inheritKw = "$(inherit)"

inheritRegex :: String
inheritRegex = "\\$\\(inherit\\)"

reservedWords :: [String]
reservedWords =   uncurry (:) usingFileKw
               ++ uncurry (:) helpKw

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
defaultIs a = defaultIf a (const True)

defaultIf :: a -> (FlagResults -> Bool) -> FlagConf a
defaultIf = FlagConf_DefaultIf

aliasIs :: [String] -> FlagConf a
aliasIs = FlagConf_Alias

requiredIf :: (FlagResults -> Bool) -> FlagConf (Maybe a)
requiredIf = FlagConf_RequiredIf

parser :: (FlagArgument -> Maybe a) -> FlagConf a
parser = FlagConf_Parser

maybeParser :: (FlagArgument -> Maybe a) -> FlagConf (Maybe a)
maybeParser p = FlagConf_Parser (maybeParserWrapper p)

maybeParserWrapper :: (FlagArgument -> Maybe a)
                   -> FlagArgument
                   -> Maybe (Maybe a)
maybeParserWrapper p arg = case arg of
    FlagMissing _      -> Just Nothing
    FlagValueMissing _ -> Just Nothing
    flagValue          -> case p flagValue of
                              Nothing -> Nothing
                              val     -> Just val

get :: FlagResults -> Flag a ->  a
get result (Flag name _ conf) = fromJust $ runParser result conf value
  where value = fromMaybe (error fatalError) $ Map.lookup name result
        fatalError = "Error while trying to get flag value for '" ++ name
                  ++ "'. Flag was not added to the flagData array"

flagDefault :: FlagResults -> [FlagConf a] -> Maybe a
flagDefault fr fc =
    case listToMaybe [ (x, p) | (FlagConf_DefaultIf x p) <- fc] of
        Just (x, p) -> if p fr then Just x else Nothing
        _           -> Nothing

flagAlias :: [FlagConf a] -> [String]
flagAlias fc = concat [ x | (FlagConf_Alias x) <- fc]

flagDAlias :: [FlagDataConf] -> [String]
flagDAlias fc = concat [ x | (FlagDataConf_Alias x) <- fc]

flagDDefaultOperation :: [FlagDataConf] -> OperationToken
flagDDefaultOperation fc =
    case [ x | (FlagDataConf_DefaultOperation x) <- fc] of
        []  -> OperationTokenAssign
        res -> head res

flagEmptyValue :: [FlagConf a] -> Maybe a
flagEmptyValue fc = listToMaybe [ x | (FlagConf_EmptyValueIs x) <- fc]

runRealParser :: [FlagConf a] -> FlagArgument -> Maybe a
runRealParser flagconf = head [x | (FlagConf_Parser x) <- flagconf]

runParser :: FlagResults -> [FlagConf a] -> FlagArgument -> Maybe a
runParser fr fc arg@(FlagMissing _) = case flagDefault fr fc of
    Nothing  -> runRealParser fc arg
    Just val -> Just val
runParser _ fc arg@(FlagValueMissing _) = case flagEmptyValue fc of
    Nothing  -> runRealParser fc arg
    Just val -> Just val
runParser _ fc arg = runRealParser fc arg

combine :: [FlagData] -> FlagData
combine = foldl combine' (Map.empty, Map.empty, [])
  where combine' (m1, a1, gr1) (m2, a2, gr2) =
            case duplicates (m1, a1) (m2, a2) of
                [] -> (m1 `Map.union` m2, a1 `Map.union` a2, gr1 ++ gr2)
                flags -> error ( "Duplicate flag names: The following flag "
                              ++ "names are duplicated in the code "
                              ++  show flags)

        allKeys (m1, m2)= Map.keys m1 ++ Map.keys m2
        duplicates (m1, a1) (m2, a2) = allKeys (m1, a1) `intersect`
                                       allKeys (m2, a2)


validate :: GlobalRule -> FlagData
validate rule = (Map.empty, Map.empty, [rule])

flagToData :: Flag a -> FlagData
flagToData flag@(Flag name help flagConf) =
    case invalidFlag flag of
        Just err -> error err
        _        -> (flagData, aliasMap, [])
  where flagData = Map.singleton name (help, flagDConf)
        aliasMap = Map.fromList [(s, name) | s <- flagAlias flagConf]

        flagDConf = map aux flagConf
        aux (FlagConf_DefaultIf _ p) = FlagDataConf_HasDefault p
        aux (FlagConf_RequiredIf p) = FlagDataConf_RequiredIf p
        aux (FlagConf_EmptyValueIs _) = FlagDataConf_HasEmptyValue
        aux (FlagConf_Parser p) = FlagDataConf_Validator (isJust . p)
        aux (FlagConf_Alias as) = FlagDataConf_Alias as
        aux (FlagConf_DefaultOperation op) = FlagDataConf_DefaultOperation op

invalidFlag :: Flag a -> Maybe String
invalidFlag (Flag n _ fc) = case invalidFlags of
    []    -> Nothing
    flags -> Just $ "Error: The following flags names are invalid "
                 ++ show flags
  where invalidFlags = [x | x <- n:flagAlias fc, invalidFlagName x]

invalidFlagName :: String -> Bool
invalidFlagName s = not (s =~ "^[a-zA-Z][a-zA-Z0-9\\-_]*$" :: Bool)

mapAlias :: FlagAliasMap -> String -> String
mapAlias aliasMap name = fromMaybe name $ Map.lookup name aliasMap

replaceStr :: String -> (String, String) -> String
replaceStr str (pattern, repl) = subRegex (mkRegex pattern) str repl

executeOp :: ParseResults
          -> (String, OperationToken, FlagValueToken)
          -> FlagArgument
executeOp st (name, op, val) = case op of
    OperationTokenAssign   -> execAssign st (name, val)
    OperationTokenAppend   -> execAppend st (name, val)
    OperationTokenAppend'  -> execAppend' st (name, val)
    OperationTokenPrepend  -> execPrepend st (name, val)
    OperationTokenPrepend' -> execPrepend' st (name, val)

execAssign :: ParseResults -> (String, FlagValueToken) -> FlagArgument
execAssign _ (name, val) = case val of
    FlagValueTokenEmpty    -> FlagValueMissing name
    (FlagValueToken value) -> FlagValue name value

execAppend :: ParseResults -> (String, FlagValueToken) -> FlagArgument
execAppend (fr, _) (name, val) = case val of
    FlagValueTokenEmpty    -> previousOrEmpty fr name
    (FlagValueToken value) -> FlagValue name (inheritKw ++ prefix ++ value)
  where prefix =  if isJust $ Map.lookup name fr then " " else ""

execAppend' :: ParseResults -> (String, FlagValueToken) -> FlagArgument
execAppend' (fr, _) (name, val) = case val of
    FlagValueTokenEmpty    -> previousOrEmpty fr name
    (FlagValueToken value) -> FlagValue name (inheritKw ++ value)

execPrepend :: ParseResults -> (String, FlagValueToken) -> FlagArgument
execPrepend (fr, _) (name, val) = case val of
    FlagValueTokenEmpty    -> previousOrEmpty fr name
    (FlagValueToken value) -> FlagValue name (value ++ prefix ++ inheritKw)
  where prefix =  if isJust $ Map.lookup name fr then " " else ""

execPrepend' :: ParseResults -> (String, FlagValueToken) -> FlagArgument
execPrepend' (fr, _) (name, val) = case val of
    FlagValueTokenEmpty    -> previousOrEmpty fr name
    (FlagValueToken value) -> FlagValue name (value ++ inheritKw)

previousOrEmpty :: FlagResults -> String -> FlagArgument
previousOrEmpty fr name = fromMaybe (FlagValueMissing name)
                                    (Map.lookup name fr)

applyInherit :: ParseResults -> FlagArgument -> FlagArgument
applyInherit (fr, _) arg = case arg of
    (FlagMissing _)         -> arg
    (FlagValueMissing _)    -> arg
    (FlagValue name value0) -> FlagValue name value1
      where value1 = value0 `replaceStr` (inheritRegex, previous)
            previous = case Map.lookup name fr of
                           Just (FlagValue _ v) -> v
                           _                    -> ""

parseToken :: (ParseResults, Token) -> ParseResults
parseToken (st, tok) = case tok of
    (ArgToken arg)             -> (Map.empty, [arg])
    (FlagToken name op value0) -> let value1 = executeOp st (name, op, value0)
                                      value2 = applyInherit st value1
                                  in (Map.singleton name value2, [])

parseArgs :: [Token] -> ParseResults -> ParseResults
parseArgs [] st0 = st0
parseArgs (tok:toks) st0 = let st1 = parseToken (st0, tok)
                               st2 = mergeSt (st0, st1)
                           in parseArgs toks st2
  where mergeSt ((fr1, args1), (fr2, args2)) = ( fr2 `Map.union` fr1
                                               , args1 ++ args2)



concatToks :: TokenizeResult -> TokenizeResult -> TokenizeResult
concatToks (Left errs) (Left errs2) = Left (errs ++ errs2)
concatToks (Left errs) _ = Left errs
concatToks _ (Left errs) = Left errs
concatToks (Right toks1) (Right toks2) = Right $ toks1 ++ toks2

readFile' :: String -> IO (Either SomeException String)
readFile' name = try $ readFile name :: IO (Either SomeException String)

parseConfFile :: ([String], FlagData)
              -> String
              -> IO TokenizeResult
parseConfFile (parents, fd) filename = do
    fileResult <- readFile' filename
    case fileResult of
        Left except -> return $ Left [errorWithFile except]
        Right content -> tokenize (parents, fd) (removeComments content)

  where errorWithFile err = FlagFatalError $ "Error on '"
                                          ++ filename
                                          ++ "': "
                                          ++ show err

removeComments :: String -> String
removeComments []     = []
removeComments (c:cs) = if c == '#'
                        then removeComments $ dropWhile (/= '\n') cs
                        else c:removeComments cs

isUsingConfFlag :: Token -> IO (Maybe String)
isUsingConfFlag (FlagToken name _ (FlagValueToken file)) =
    if name `notElem` uncurry (:) usingFileKw
    then return Nothing
    else liftM Just (canonicalizePath file)
isUsingConfFlag _ =  return Nothing

includeConfig :: ([String], FlagData) -> [Token] -> IO TokenizeResult
includeConfig _ [] = return $ Right []
includeConfig (parents, flags) (t:ts) = do
    isUsingConf <- isUsingConfFlag t
    case isUsingConf of
        Nothing -> do rest <- includeConfig (parents, flags) ts
                      return $ Right [t] `concatToks` rest
        Just conf -> let parents' = parents ++ [conf] in
                     if conf `elem` parents
                     then reportCircularDependency parents'
                     else do confToks <- parseConfFile (parents', flags) conf
                             restToks <- includeConfig (parents, flags) ts
                             return $ confToks `concatToks` restToks

reportCircularDependency :: [String] -> IO TokenizeResult
reportCircularDependency files = return (Left [FlagFatalError msg])
  where msg = "Error while parsing conf file: "
           ++ "Circular includes on files\n"
           ++ format files
        format fs = case fs of
                        []     -> ""
                        [x]    -> "  " ++ x
                        (x:xs) -> "  " ++ x ++ " ->\n" ++ format xs

tokenize :: ([String], FlagData) -> String -> IO TokenizeResult
tokenize (parents, flags) input = includeConfig (parents, flags) toks
  where (fd, _, _) = flags
        defaultOp = mkDefaultOp $ Map.toList fd
        toks = parseInput defaultOp input

mkDefaultOp :: [(String, FlagDatum)] -> DefaultOp
mkDefaultOp [] = Map.empty
mkDefaultOp (x:xs) = Map.singleton name defaultOp `Map.union` defaultOps
  where (name, (_, flagDataConf)) = x
        defaultOps = mkDefaultOp xs
        defaultOp = flagDDefaultOperation flagDataConf

process :: FlagData -> [String] -> IO (Either [FlagError] ProcessResults)
process fd args = do result <- tokenize ([], fd) (unwords args)
                     case result of
                         Left errs -> return $ Left errs
                         Right toks -> return $ process' fd toks

process' :: FlagData -> [Token] -> Either [FlagError] ProcessResults
process' fd toks =
    case pipelines [localValidation, globalValidation] (fd, flags) of
        ([], res) -> Right (res, args)
        (errs, _) -> Left errs
  where toks' = updateFlagAlias fd toks
        (flags, args) = parseArgs toks' (Map.empty, [])
        localValidation = [ validateReservedWords
                          , addMissingFlags
                          , validateUnknownFlags
                          , validateFlagParsers]
        globalValidation = [ validateRequiredIf
                           , validateDependentDefault
                           , validateGlobalRules]

updateFlagAlias :: FlagData -> [Token] -> [Token]
updateFlagAlias (_, aliasMap, _) = map updateAlias
  where updateAlias tok = case getFlagName tok of
                              Nothing   ->  tok
                              Just name -> let name' = mapAlias aliasMap name
                                           in updateName tok name'

getFlagName :: Token -> Maybe String
getFlagName tok = case tok of
                      (FlagToken name _ _) -> Just name
                      _                    -> Nothing

updateName :: Token -> String -> Token
updateName tok name = case tok of
                          (FlagToken _ op value) -> FlagToken name op value
                          _                      -> tok

anyArgIsHelp :: [String] -> Bool
anyArgIsHelp args = any (`elem` args) helpFlags
  where helpFlags = concat [["--" ++ x, "-" ++ x] | x <- uncurry (:) helpKw]

processMain :: String
            -> FlagData
            -> (ProcessResults -> IO ())
            -> ([FlagError] -> IO ())
            -> (String -> [(String, [String], String)] -> IO ())
            -> IO ()
processMain desc flags success failure displayHelp = do
    args <- getQuotedArgs
    if anyArgIsHelp args
      then displayHelp desc $ getFlagHelp flags
      else do result <- process flags args
              case result of
                  Left errs -> failure errs
                  Right res -> success res

getQuotedArgs  :: IO [String]
getQuotedArgs = do args <- getArgs
                   return $ map quote' args
  where quote' s = if length (words s) > 1
                   then "\"" ++ s ++ "\""
                   else s

hasFatalError :: [FlagError] -> Bool
hasFatalError errs = not $ null [True | FlagFatalError _ <- errs]

pipelines :: [[PipelineFunction]] -> PipelineFunction
pipelines [] (_, flags)      = ([], flags)
pipelines (p:ps) (fd, flags) = case pipeline p (fd, flags) of
                                   ([], res) -> pipelines ps (fd, res)
                                   errs      -> errs

pipeline :: [PipelineFunction] -> PipelineFunction
pipeline [] (_, flags) = ([], flags)
pipeline (val:vs) (fd, flags0) = case val (fd, flags0) of
    ([], flags1)    -> pipeline vs (fd, flags1)
    (errs1, flags1) -> if hasFatalError errs1
                       then (errs1, flags1)
                       else let (errs2, flags2) = pipeline vs (fd, flags1)
                            in (errs1 ++ errs2, flags2)

validateReservedWords :: PipelineFunction
validateReservedWords ((fd, aliasMap, _), flags) =
    case reservedWords `intersect` codeFlags of
        []    -> ([], flags)
        names -> (map reservedWordsError names, flags)
  where codeFlags = Map.keys fd ++ Map.keys aliasMap
        errorMsg = "The name is a reserved word and can not be used"
        reservedWordsError name = FlagFatalError $ flagError name errorMsg

ifIsJust :: Maybe a -> (a -> Bool) -> Bool
ifIsJust val predicate = case val of
                            Nothing -> True
                            Just a  -> predicate a

addMissingFlags :: PipelineFunction
addMissingFlags ((fd, aliasMap, _), flags) = ([], flags')
  where flags' = flags `Map.union` Map.fromList missingFlags'
        inputFlags = Map.keys flags
        codeFlags = Map.keys fd
        missingFlags = [x | x <- codeFlags
                          , x `notElem` inputFlags
                          , ifIsJust (Map.lookup x aliasMap)
                                     (`notElem` inputFlags)
                       ]
        missingFlags' = map (\name -> (name, FlagMissing name)) missingFlags

validateUnknownFlags :: PipelineFunction
validateUnknownFlags ((fd, aliasMap, _), flags) = (errors, flags)
  where inputFlags = Map.keys flags
        codeFlags = Map.keys fd ++ Map.keys aliasMap
        missingFlags = inputFlags \\ codeFlags
        errors = map (FlagFatalError . flagUnkownError) missingFlags
        errorMsg = "Unkown flag is not defined in the code"
        flagUnkownError name = flagError name errorMsg

validateFlagParsers :: PipelineFunction
validateFlagParsers ((fd, _, _), flags) =
    (mapMaybe aux (Map.toList fd), flags)
  where aux (name, (_, conf)) = case checkValidator conf value of
            ValidationError err -> Just err
            _                   -> Nothing
          where value = fromJust (Map.lookup name flags)

validateRequiredIf :: PipelineFunction
validateRequiredIf ((fd, _, _), flags) = (mapMaybe aux (Map.toList fd), flags)
  where aux (name, (_, flagDataConf)) =
            case requiredIfValidator flagDataConf flags value of
                ValidationError err -> Just err
                _                   -> Nothing
          where value = fromJust (Map.lookup name flags)

validateDependentDefault :: PipelineFunction
validateDependentDefault ((fd, _, _), flags) =
    (mapMaybe aux (Map.toList fd), flags)
  where aux (name, (_, flagDataConf)) =
            case defaultIfValidator flagDataConf flags value of
                ValidationError err -> Just err
                _                   -> Nothing
          where value = fromJust (Map.lookup name flags)

validateGlobalRules :: PipelineFunction
validateGlobalRules ((_, _, gr), flags) = (flagErrs, flags)
  where errs = mapMaybe (\ r -> r flags) gr
        flagErrs = map FlagNonFatalError errs

requiredIfValidator :: [FlagDataConf]
                    -> FlagResults
                    -> FlagArgument
                    -> ValidationResult
requiredIfValidator fdc fr (FlagMissing name)
  | flagDIsRequiredIf fdc fr = validationError name "Flag is required"
  | otherwise = ValidationSuccess
requiredIfValidator _ _ _ = ValidationSuccess

defaultIfValidator :: [FlagDataConf]
                   -> FlagResults
                   -> FlagArgument
                   -> ValidationResult
defaultIfValidator fdc fr (FlagMissing name)
  | flagDHasDefault fdc = if flagDGetDefaultIf fdc fr || flagDIsOptional fdc
                          then ValidationSuccess
                          else validationError name "Flag is required"
  | otherwise = ValidationSuccess
defaultIfValidator _ _ _ = ValidationSuccess

make :: (String, String, [FlagConf a]) -> Flag a
make (name, help, flagConf) = if hasParser
                              then Flag name help flagConf
                              else error $ flagError name errorMsg
  where hasParser = not $ null [True | (FlagConf_Parser _) <- flagConf]
        errorMsg = "Flag parser was not provided"

defaultDisplayHelp :: String -> [(String, [String], String)] -> IO ()
defaultDisplayHelp desc flags = putStrLn helpText
  where helpText = Opt.usageInfo desc (map getOptDescr flags)
        getOptDescr (name, alias, help) = mapOptOption (name:alias) help

mapOptOption :: [String] -> String -> Opt.OptDescr String
mapOptOption names = Opt.Option short long (Opt.NoArg "")
  where (short, long) = splitShortName names

splitShortName :: [String] -> (String, [String])
splitShortName = foldl aux ([], [])
  where aux (s, l) current = if length current == 1
                             then (s ++ [head current], l)
                             else (s, l ++ [current])

defaultDisplayErrors :: [FlagError] -> IO ()
defaultDisplayErrors errs = do putStrLn "Errors occurred while parsing flags:"
                               mapM_ print errs

getFlagHelp :: FlagData -> [(String, [String], String)]
getFlagHelp (fd, _, _) = helps ++ [usingFileHelpText] ++ [helpHelpText]
  where helps = map aux (Map.toList fd)
        aux (name, (help, conf)) = (name, flagDAlias conf, help)

        usingFileHelpText = ( fst usingFileKw
                            , snd usingFileKw
                            , "read flags from configuration file"
                            )
        helpHelpText = ( fst helpKw
                       , snd helpKw
                       , "show this help"
                       )

flagDIsOptional :: [FlagDataConf] -> Bool
flagDIsOptional fdc = not $ null [True | (FlagDataConf_RequiredIf _) <- fdc]

flagDIsRequiredIf :: [FlagDataConf] -> FlagResults -> Bool
flagDIsRequiredIf fdc fr = case res of
                              Nothing -> False
                              Just p  -> p fr
   where res = listToMaybe [ p | (FlagDataConf_RequiredIf p) <- fdc]

flagDGetDefaultIf :: [FlagDataConf] -> FlagResults -> Bool
flagDGetDefaultIf fdc fr = case def of
                               Just p  -> p fr
                               Nothing -> False
   where def = listToMaybe [ p | (FlagDataConf_HasDefault p) <- fdc]

flagDHasDefault :: [FlagDataConf] -> Bool
flagDHasDefault fdc = not $ null [ True | (FlagDataConf_HasDefault _) <- fdc]

flagDHasEmptyValue :: [FlagDataConf] -> Bool
flagDHasEmptyValue fdc = not $ null [ True | FlagDataConf_HasEmptyValue <- fdc]

runDValidator :: [FlagDataConf] -> FlagArgument -> Bool
runDValidator fdc = validator
  where validator = head [x | (FlagDataConf_Validator x) <- fdc]

validationError :: String -> String -> ValidationResult
validationError name s = ValidationError $ FlagNonFatalError errorMsg
  where errorMsg = flagError name s

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

valueParser :: Read a => FlagArgument -> Maybe a
valueParser arg = case arg of
                      (FlagMissing _)      -> Nothing
                      (FlagValueMissing _) -> Nothing
                      (FlagValue _ value)  -> readMaybe value

intParser :: FlagArgument -> Maybe Int
intParser = valueParser

doubleParser :: FlagArgument -> Maybe Double
doubleParser = valueParser

floatParser :: FlagArgument -> Maybe Float
floatParser = valueParser

arrayParser :: Read a => FlagArgument -> Maybe [a]
arrayParser = valueParser

charParser :: FlagArgument -> Maybe Char
charParser arg = case arg of
                     (FlagMissing _)      -> Nothing
                     (FlagValueMissing _) -> Nothing
                     (FlagValue _ value)  -> if length value /= 1
                                             then Nothing
                                             else Just $ head value

stringParser :: FlagArgument -> Maybe String
stringParser arg = case arg of
                       (FlagMissing _)      -> Nothing
                       (FlagValueMissing _) -> Nothing
                       (FlagValue _ value)  -> Just value

boolParser :: FlagArgument -> Maybe Bool
boolParser arg = case arg of
                     (FlagMissing _)      -> Just False
                     (FlagValueMissing _) -> Just True
                     (FlagValue _ value)  -> readMaybe value

boolFlag :: [FlagConf Bool]
boolFlag = [ parser boolParser
           , defaultIs False
           , emptyValueIs True
           ]

{-# ANN module "HLint: ignore Use camelCase" #-}
