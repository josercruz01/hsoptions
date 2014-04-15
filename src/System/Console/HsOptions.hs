{- |
Module      :  System.Console.HsOptions
Description :  Command line flag parser for Haskell
Copyright   :  (c) Jose Raymundo Cruz (jose.r.cruz01@gmail.com)
License     :  Apache-2.0

Maintainer  :  Jose Raymundo Cruz (jose.r.cruz01@gmail.com)
Stability   :  stable
Portability :  portable

@HsOptions@ library supports command line flag parsing.

Flags are declared in the code by using the 'make' function, which takes the
flag's name, help text and type as arguments.

The flags are parsed from the command line stream of from a file
if the @--usingFile \<filename\>@ flag is sent to the program.

Flags can be customized by calling configuration function, such as
'defaultIs' or 'aliasIs', that change how the flag behaves, how it
is parsed and validated.

The 'processMain' function needs to be called at the beginning of the 'main'
function. This function takes as arguments:

    * The @program description@

    * A list of @all declared flags@

    * Success callback function

    * Failure callback function

    * Display-Help callback function

If there is any kind of validation error @failure@ is
called with the list of errors. If the @--help@ flag was sent by the user
@display help@ is called. Otherwise if there are no problems the @success@
function is called.

A default implementation of @failure@ and @display help@ is provided in the
library ('defaultDisplayHelp', 'defaultDisplayErrors') with a basic behavior.

Basically, @success@ becomes the \'real\' main function. It takes as argument
a tuple ('FlagResults', 'ArgsResults'). 'FlagResults' is a data structure
that can be used to query flags by using the 'get' function. 'ArgsResults' is
just an array of 'String' containing the remaining not-flag arguments.

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

At the 'processMain' function each of the input flags is validated against the
declared flags. Within the @success@ function you can be sure that all required
flags exist, all flag types are correct and all validation was successful.
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

    -- * Default functions
    defaultDisplayHelp,
    defaultDisplayErrors,

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
    prepend,
    prepend',

    -- * Data types
    Flag,
    FlagData,
    FlagError(..),
    FlagResults,
    FlagArgument(..),
    GlobalRule,
    ProcessResults,
    ArgsResults
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

-- | Data type that represents a defined flag.
-- It contains:
--
--    * the name of the flag
--    * help text for the flag
--    * list of configurations for the flags such as type, default values, etc.
data Flag a = Flag String String [FlagConf a]

-- | Data type for a flag error. It will contain the error message and
-- what kind of error occurred:
--
--    * FatalError: an error that the system can't recover from
--    * NonFatalError: an error that does not stop the process
data FlagError = FlagNonFatalError { fNFerror :: String }
               | FlagFatalError { fFerror :: String }

-- | Type that represents a collection of all defined flags.
--
-- It has three components:
--
--    * A flag map: The key is the flag name and the value is the flag data
--    * An alias map: A map that connects any flag alias with it's unique flag
--    name. This is used to convert each flag alias to it's flag name when
--    parsing.
--    * A list of global validation rules
type FlagData = (FlagDataMap, FlagAliasMap, [GlobalRule])

-- | Type that holds a collection of all flags.
--
-- It's a map from flag name to flag data. The flag data holds all defined
-- configuration for each flag such as flag type, parser, default, etc.
type FlagDataMap = Map.Map String (String, [FlagDataConf])

-- | Type that holds a map from flag alias to flag name.
--
-- It is used to identify the corresponding flag given a flag alias. For
-- example if the user_id flag has two alias, @u@ and @uid@, this map will
-- have these entries: { @uid@ => @user_id@, @u@ => @user_id@ }.
type FlagAliasMap = (Map.Map String String)

-- | Type that represents the final result of the parse process.
--
-- It maps a flag name to it's value. This value is of type 'FlagArgument',
-- which means that it can be empty or not.
--
-- This type is used by the user to get each flag value in the main method by
-- using the 'get' method and passing a flag variable.
type FlagResults = (Map.Map String FlagArgument)

-- | Data type that represents an input flag argument.
--
-- It's type will vary depending on the user input. For example if the user
-- calls the program that expects the @user_id@ flag:
--
-- >>> ./runhaskell Program.hs
-- FlagArgument = FlagMissing "user_id"
--
-- >>> ./runhaskell Program.hs --user_id
-- FlagArgument = FlagValueMissing "user_id"'
--
-- >>> ./runhaskell Program.hs --user_id 8
-- FlagArgument = FlagValue "user_id" "8"
--
data FlagArgument = FlagMissing String -- ^ argument not provided
                  | FlagValueMissing String -- ^ argument provided but
                                            -- not it's value
                  | FlagValue String String -- ^ argument with value
                                            -- provided

-- | Type that is the list of remaining positional arguments after the parse
-- process is completed. For example:
--
-- > ./runhaskell Program.hs --user_id 8 one two three
--
-- 'ArgsResults' will contain the list [\"one\", \"two\", \"three\"]
type ArgsResults = [String]


-- | Type that holds the 'FlagResults' and 'ArgsResults' together.
type ParseResults  = (FlagResults, ArgsResults)

-- | Type of the return value of the 'process' function and it's sub-functions.
type ProcessResults  = (FlagResults, ArgsResults)

-- | Type that represents a pipeline validation/processing function.
--
-- It takes a previous state as a parameter and does a set of modifications
-- to this state.
--
-- It returns a list of errors (if any error occurred) and
-- a modified state that will be passed in to the next function in the
-- pipeline.
type PipelineFunction = (FlagData, FlagResults) -> ([FlagError], FlagResults)

-- | Type that represents a global validation rule for a 'FlagResults'.
--
-- It is used to create global validation after the flags are processed.
type GlobalRule = FlagResults -> Maybe String

-- | Type that represents the result of the 'tokenize' function and it's
-- sub-functions.
--
-- It returns either a list of errors or a valid list of tokens.
type TokenizeResult = Either [FlagError] [Token]

-- | Type that specifies whether a given validation was successful or not.
--
-- If it was not successful it contains a 'FlagError' that explains what
-- failed.
data ValidationResult =
    ValidationError FlagError
  | ValidationSuccess

-- | Data type that represent a flag configuration.
--
-- It is used when a flag is created to set the type of the flag, how it is
-- parsed, if the flag is required or optional, etc.
data FlagConf a =
    -- | Function that parses the input value of the flag to it's
    -- corresponding type, see 'charParser' for an example of this
    -- type of function.
    --
    -- The flag input text is of type 'FlagArgument', so you can determine
    -- how to map the value if it's missing or it's value is missing or if
    -- it's value was provided.
    --
    -- The function returns a 'Maybe a' type,
    -- @'Nothing'@ if the string value cannot be parsed from the input text and
    -- @'Just'@ value if it can be parsed.
    FlagConf_Parser (FlagArgument -> Maybe a)

    -- | Function that sets a dependent default value to the flag.
    --
    -- If the flag was not provided by the user this will be the default
    -- value for the flag if the predicate returns true.
  | FlagConf_DefaultIf a (FlagResults -> Bool)

    -- | Function that given a 'FlagResults' constraints the flag to be
    -- either required or not required.
    --
    -- If this function returns true then the flag will be required, and if
    -- not present a @flag is required@ message will be displayed to the user.
    --
    -- If this function returns false then the flag presence will be ignored.
  | FlagConf_RequiredIf (FlagResults -> Bool)

    -- | Default value for the flag when the flag was provided by the user
    -- but not the flag value (@i.e. runhaskell Program.hs --user_id@).
    --
    -- In this example @user_id@ will take the default value configured with
    -- this flag configuration since it's value is 'FlagValueMissing'
  | FlagConf_EmptyValueIs a

    -- | Alias for the flags. Allows the user to specify multiple names for
    -- the same flag, such as short name or synonyms.
  | FlagConf_Alias [String]

    -- | Default operation for flag when no operation is specified.
    --
    -- >>> runhaskell Program.hs --user_name += batman
    -- Operation was specified: Operation = "Append value"
    --
    -- >>> runhaskell Program.hs --user_name batman
    -- Operation not specified: Operation = 'FlagConf_DefaultOperation'
  | FlagConf_DefaultOperation OperationToken

-- | Data type that represents a generic flag type configuration.
--
-- It is mapped from the 'FlagConf' of each flag so that it can be bundled
-- together with all other flag's data. It is used at the validation/parsing
-- phase of the 'process' method to verify that the input flag value is
-- valid for each flag (i.e. if a required flag was not provided by the user
-- but this flag has a default value then an error does not occur).
--
-- It has a direct mapping of each 'FlagData' to a non-generic version.
data FlagDataConf =
    -- | Determines if a flag value is valid for a given flag.
    --
    -- Corresponds to 'FlagConf_Parser' and returns 'True' if the result value
    -- of the 'FlagConf_Parser' is 'Just', returns 'False' otherwise
    FlagDataConf_Validator (FlagArgument -> Bool)

    -- | Determines if a flag has a dependent default value configured.
    --
    -- Corresponds just to the predicate part of 'FlagConf_DefaultIf'.
  | FlagDataConf_HasDefault (FlagResults -> Bool)

    -- | Exactly the same as 'FlagConf_RequiredIf'
  | FlagDataConf_RequiredIf (FlagResults -> Bool)

    -- | Determines if a flag has a @empty value@ configured.
    --
    -- It is mapped from an 'FlagConf_EmptyValueIs'.
  | FlagDataConf_HasEmptyValue

    -- | Exactly the same as 'FlagConf_Alias'
  | FlagDataConf_Alias [String]

    -- | Exactly the same as 'FlagConf_DefaultOperation'
  | FlagDataConf_DefaultOperation OperationToken

-- | Making 'FlagError' an instance of 'Show'
instance Show FlagError where
  -- | To show a 'FlagFatalError' we just return the error message
  show (FlagFatalError err) = err

  -- | To show a 'FlagNonFatalError' we just return the error message
  show (FlagNonFatalError err) = err

-- | Takes a flag's name and an error message and builds a proper error
-- for the flag.
--
-- Arguments:
--
--    * @flag_name@: the name of the flag
--
--    * @error@: the error message
--
-- Returns:
--
--    * A pretty error message for the flag name.
flagError :: String -> String -> String
flagError name msg = "Error with flag '--" ++ name ++ "': " ++ msg

-- | The keyword for the @usingFile@ flag and its aliases.
--
-- This flag is used to include a configuration text file for flag parsing.
usingFileKw :: (String, [String])
usingFileKw = ("usingFile", [])

-- | The keyword for the @help@ flag and its aliases.
--
-- When the user pass in this flag the @display error@ function is called.
helpKw :: (String, [String])
helpKw = ("help", ["h"])

-- | The keyword for the @inherit@ feature.
--
-- This keyword allows the user to set a flag's value based on it's previous
-- value.
inheritKw :: String
inheritKw = "$(inherit)"

-- | Regex used to match the 'inheritKw'.
inheritRegex :: String
inheritRegex = "\\$\\(inherit\\)"

-- | Holds all words that are reserved on by the library and cannot be used
-- as flag names.
reservedWords :: [String]
reservedWords =   uncurry (:) usingFileKw
               ++ uncurry (:) helpKw

-- | Constructor for flag configuration ('FlagConf').
--
-- Marks a flag as optional. Since the flag is optional then it's type must
-- bet 'Flag' ('Maybe' a) because it will be 'Nothing' if the flag is not
-- provided and @'Just' value@ if it is.
--
-- Returns:
--
--    * A flag configuration that marks the flag as optional.
--      This method is a specification of the 'requiredIf' method. Is is
--      equivalent to:
--
--      >>> requiredIf (const False)
isOptional :: FlagConf (Maybe a)
isOptional = requiredIf (const False)

-- | Constructor for the 'FlagConf_EmptyValueIs'.
--
-- Sets the value a flag should take if it is the case that this flag was
-- provided by the user but not it's value (@i.e. runhaskell Program.hs
-- --user_id@).
--
-- Arguments:
--
--    * @empty_value@: the value to use if the flag value is empty.
--
-- Returns:
--
--    * A flag configuration that sets the flag's empty value.
emptyValueIs :: a -> FlagConf a
emptyValueIs = FlagConf_EmptyValueIs

-- | Constructor for the 'FlagConf_DefaultIf'.
--
-- Sets the default value for a flag if the flag is not provided by the user.
--
-- Arguments:
--
--    * @default value@: the default value of the flag
--
-- Returns:
--
--    * A flag configuration that sets the flag's default value.
--      This method is a specification of the 'defaultIs' method. Is is
--      equivalent to:
--
--      >>> defaultIf a (const True)
defaultIs :: a -> FlagConf a
defaultIs a = defaultIf a (const True)

-- | Constructor for the 'FlagConf_DefaultIf'.
--
-- Sets a dependent default value for a flag. This default value will be used
-- if the predicate returns 'True' and the flag is not provided by the user.
--
-- Arguments:
--
--    *@default_value@: the dependent default value for the flag
--
--    *@predicate@: the predicate that indicates if the default value should
--    be used. If this returns 'True' then this default value will be the
--    flag's value if the flag is not provided by the user.
--
-- Returns:
--
--    * A flag configuration that sets the dependent default value for the
--    flag.
defaultIf :: a -> (FlagResults -> Bool) -> FlagConf a
defaultIf = FlagConf_DefaultIf

-- | Constructor for the 'FlagConf_Alias'
--
-- Sets multiple alias for a single flag. @(i.e. --user_id alias:
-- [\"u\", \"uid\",\"user_identifier\"])@. These aliases can be used to set
-- the flag value, so @--user_id = 8@ is equivalent to @-u = 8@.
--
-- Arguments:
--
--    *@aliases@: the alias list for the flag.
--
-- Returns:
--
--    * A flag configuration that sets the aliases for a given flag.
aliasIs :: [String] -> FlagConf a
aliasIs = FlagConf_Alias

-- | Constructor for the 'FlagConf_RequiredIf'.
--
-- Marks a flag conditionally required. This is that the flag will be required
-- from the user if the condition set here returns 'True'. If the user does
-- not provides the flag and this condition returns 'True' then a \"flag is
-- required\" error is displayed to the user.
--
-- The flag type must be @'Maybe' a@, if the @condition@ returns 'False' and
-- the flag is not provided then it's value is 'Nothing'. On the other hand
-- if the flag is provided it's value will be @'Just' value@.
--
-- Arguments:
--
--    *@condition@: the condition to determine if the flag is required.
--
-- Returns:
--
--    * A flag configuration that sets the conditional flag required
--    constraint.
requiredIf :: (FlagResults -> Bool) -> FlagConf (Maybe a)
requiredIf = FlagConf_RequiredIf

-- | Constructor for the 'FlagConf_Parser'.
--
-- Sets the function that will parse the string input to the corresponding
-- flag type value.
--
-- Arguments:
--
--    *@parser_function@: a function that takes a flag argument and returns
--    a @'Maybe' value@ if the argument can be parsed to the flag type.
--
-- Returns:
--
--    * A flag configuration that sets how to parse the string input.
parser :: (FlagArgument -> Maybe a) -> FlagConf a
parser = FlagConf_Parser

-- | Wrapper for 'parser'. It's similar to 'parser' but it returns a
-- @'FlagConf' ('Maybe' a)@ instead of a 'FlagConf a'.
--
-- The @parser_function@ argument is wrapped so that it doesn't fails if the
-- 'FlagArgument' is missing.
--
-- It is a covenient way to reuse current parsers without having to redefine
-- them. For instance \"'intParser'\" is of type
-- @'FlagArgument' ('Maybe' 'Int')@, \"@maybeParser intParser@\" instead, is of
-- type @'FlagArgument' ('Maybe' ('Maybe' 'Int'))@.
--
-- For example:
--
-- @
-- user_id :: 'Flag' 'Int'
-- user_id = 'make' (\"user_id\", \"help\", ['parser' 'intParser'])
-- user_id2 :: 'Flag' ('Maybe' 'Int')
-- user_id2 = 'make' (\"user_id2\", \"help\", ['maybeParser' 'intParser'])
-- @
--
-- Arguments:
--
--    *@parser_function@: parser function that defines how to parse the
--    string input to the flag's value.
--
-- Returns:
--
--    * A flag configuration that sets how to parse the string input.
maybeParser :: (FlagArgument -> Maybe a) -> FlagConf (Maybe a)
maybeParser p = FlagConf_Parser (maybeParserWrapper p)

-- | Constructor for 'FlagConf_DefaultOperation'.
--
-- Defines the default operation for the flag if no operation is made
-- explicit by the user. Check documentation for 'FlagConf_DefaultOperation'
-- to see more details.
--
-- Arguments:
--
--    *@default_operation@: operation to use if no operation is provided for
--    the flag in the input stream.
--
-- Returns:
--
--    * A flag configuration that sets the default operation of the flag.
operation :: OperationToken -> FlagConf a
operation = FlagConf_DefaultOperation

-- | Append operation (+=). One of the available flag operations.
--
-- The flag value is appended with it's previous value using a space in
-- between values. It is used as the argument of 'operation' function:
--
-- >>> operation append
-- Sets the default operation for the flag to append
--
-- Returns:
--
--    * Append flag operation.
append :: OperationToken
append = OperationTokenAppend

-- | Append\' operation (+=!). One of the available flag operations.
--
-- Same as 'append' but appends with no space in between. It is used as
-- the argument of 'operation' function:
--
-- >>> operation append'
-- Sets the default operation for the flag to append'
--
-- Returns:
--
--    * Append\' flag operation.
append' :: OperationToken
append' = OperationTokenAppend'

-- | Prepend operation (=+). One of the available flag operations.
--
-- The flag value is prepended with it's previous value using a space in
-- between values. It is used as the argument of 'operation' function:
--
-- >>> operation prepend
-- Sets the default operation for the flag to prepend
--
-- Returns:
--
--    * Prepend flag operation.
prepend :: OperationToken
prepend = OperationTokenPrepend

-- | Prepend\' operation (=+!). One of the available flag operations.
--
-- Same as 'prepend' but appends with no space in between. It is used as
-- the argument of 'operation' function:
--
-- >>> operation prepend'
-- Sets the default operation for the flag to prepend'
--
-- Returns:
--
--    * Prepend\' flag operation.
prepend' :: OperationToken
prepend' = OperationTokenPrepend'

-- | Assign operation (=). Default flag operation if no operation is set.
--
-- Sets the flag value to the current value, overwriting any previous value
-- the flag may have.
--
-- >>> operation assign
-- Sets the default operation for the flag to assign
--
-- Returns:
--
--    * Assign flag operation.
assign :: OperationToken
assign = OperationTokenAssign

-- | Wraps a parser function that takes a 'FlagArgument' and returns a
-- \"@'Maybe' a@\" and converts it to a function that returns a
-- \"@'Maybe' ('Maybe' a)@\".
--
-- This new function does will never fail if the argumentn is missing or
-- if the argument value is missing ('FlagMissing' or 'FlagValueMissing'),
-- instead this function maps any of these two to a 'Nothing' value.
--
-- If the flag argument is of type 'FlagValue' then the original parser
-- is used.
--
-- Arguments:
--
--    *@original_parser@: the original parser to be wrapped.
--
-- Returns:
--
--    * A new parser that changes the result type to an optional type.
maybeParserWrapper :: (FlagArgument -> Maybe a)
                   -> FlagArgument
                   -> Maybe (Maybe a)
maybeParserWrapper p arg = case arg of
    FlagMissing _      -> Just Nothing
    FlagValueMissing _ -> Just Nothing
    flagValue          -> case p flagValue of
                              Nothing -> Nothing
                              val     -> Just val

-- | Method to get a flag value out of a 'FlagResults'.
--
-- This is the method used to get the proper value for each flag after the
-- input stream has been processed. The 'processMain' method will create the
-- 'FlagResults' data structure for a set of defined flags.
--
-- This is an example on how to use this method:
--
-- > user_id :: Flag Int
-- > user_id = make ("user_id", "help", [parser intParser]
-- >
-- > main_success :: (FlagResults, ArgsResults) -> IO ()
-- > main_success (flags, _) = putStrLn ("Next user id: " ++
-- >                                    show ((get flags user_id) + 1)))
--
-- Arguments:
--
--    *@flag_results@: the 'FlagResults' created for the input stream.
--
-- Returns:
--
--    * The value of the given flag.
--
-- Throws:
--
--    * An exception is raised if flag does not exist in the 'FlagResults'.
--    At this point the 'get' method should always succeed parsing a flag
--    value. If the flag is not found that means that the flag was not
--    processed by the parser, possibly because the flag was not added to the
--    'FlagData' sent to the processor.
get :: FlagResults -> Flag a ->  a
get result (Flag name _ conf) = fromJust $ runParser result conf value
  where value = fromMaybe (error fatalError) $ Map.lookup name result
        fatalError = "Error while trying to get flag value for '" ++ name
                  ++ "'. Flag was not added to the flagData array"

-- | Finds the default value from a flag's list of configurations if the
-- default value was configured for the flag.
--
-- If a dependent default value was configured for the flag then it is matched
-- agains the @flag_results@, if the match returns 'True' then the default
-- value is returned, otherwise 'Nothing' is returned.
--
-- Arguments:
--
--    *@flag_results@: the current 'FlagResults'.
--
--    *@configurations@: the list of configurations for the flag.
--
-- Returns:
--
--    * Nothing: if there is no dependent default configuration.
--
--    * Nothing: if the depedent default configuration predicate returns
--    'False'
--
--    * Just @defaultValue@: if the dependent default configuration predicate
--    returns 'True'
flagDefault :: FlagResults -> [FlagConf a] -> Maybe a
flagDefault fr fc =
    case listToMaybe [ (x, p) | (FlagConf_DefaultIf x p) <- fc] of
        Just (x, p) -> if p fr then Just x else Nothing
        _           -> Nothing

-- | Returns the list of flag alias configured for the flag.
--
-- Arguments:
--
--    *@configurations@: the list of configurations for the flag.
--
-- Returns:
--
--    * A list of all the flag alias configured for the flag.
flagAlias :: [FlagConf a] -> [String]
flagAlias fc = concat [ x | (FlagConf_Alias x) <- fc]

-- | Returns the list of flag alias configured for the flag.
--
-- Similar to 'flagAlias' but for 'FlagDataConf' instead.
--
-- Arguments:
--
--    *@configurations@: the list of configurations for the flag.
--
-- Returns:
--
--    * A list of all the flag alias configured for the flag.
flagDAlias :: [FlagDataConf] -> [String]
flagDAlias fc = concat [ x | (FlagDataConf_Alias x) <- fc]

-- | Finds the default operation for the flag.
--
-- If the default operation is not found the the 'assign' operation is used
-- as the default.
--
-- Arguments:
--
--    *@configurations@: the list of configurations for the flag.
--
-- Returns:
--
--    * The default operation for the flag.
flagDDefaultOperation :: [FlagDataConf] -> OperationToken
flagDDefaultOperation fc =
    case [ x | (FlagDataConf_DefaultOperation x) <- fc] of
        []  -> OperationTokenAssign
        res -> head res

-- | Finds the value to use if the flag value is empty.
--
-- Arguments:
--
--    *@configurations@: the list of configurations for the flag.
--
-- Returns:
--
--    * Nothing: if there is no empty value configured for the flag
--    * Just value: if the empty value is found.
flagEmptyValue :: [FlagConf a] -> Maybe a
flagEmptyValue fc = listToMaybe [ x | (FlagConf_EmptyValueIs x) <- fc]

-- | Finds the parser for the flags and runs it against the flag argument.
--
-- Arguments:
--
--    *@configurations@: the list of configurations for the flag.
--
--    *@flag_argument@: the argument of the flag.
--
-- Returns:
--
--    * The parsed value of the @flag_argument@ by using the original flag
--    parser
--
-- Throws:
--
--    * An exception if no parser is found. This method depend on previous
--    validation that the parser was configured for the flag.
runRealParser :: [FlagConf a] -> FlagArgument -> Maybe a
runRealParser flagconf = head [x | (FlagConf_Parser x) <- flagconf]

-- | Runs the parser for the flag depending on the type of 'FlagArgument'.
--
-- Takes into considerations scenarios where the flag argument is missing but
-- the user defined a default value for the flag or if the flag argument is
-- empty and the user defined an empty value for the flag.
--
-- Arguments:
--
--    *@flag_results@: the current 'FlagResults'.
--
--    *@configurations@: the list of configurations for the flag.
--
--    *@flag_argument@: the argument of the flag.
--
-- Returns:
--
--    * The default value for the flag: if a default value was configured and
--    the flag argument is 'FlagMissing'.
--
--    * The empty value for the flag: if an empty value was configured and
--    the flag argument is 'FlagValueMissing'
--
--    * Otherwise: The result of the real parser configured for the flag.
runParser :: FlagResults -> [FlagConf a] -> FlagArgument -> Maybe a
runParser fr fc arg@(FlagMissing _) = case flagDefault fr fc of
    Nothing  -> runRealParser fc arg
    Just val -> Just val
runParser _ fc arg@(FlagValueMissing _) = case flagEmptyValue fc of
    Nothing  -> runRealParser fc arg
    Just val -> Just val
runParser _ fc arg = runRealParser fc arg

-- | Takes a list of 'FlagData' and combines them together into a single
-- 'FlagData'.
--
-- Validates that a flag name is not repeated in the incoming list of
-- flag data.
--
-- Usage example:
--
-- > flagData = combine [ flagToData user_id
-- >                    , flagToData user_name,
-- >                    , flagToData database
-- >                    ]
--
-- Arguments:
--
--    * @flag_data_list@: list of flag data to combine.
--
-- Returns:
--
--    * A combined 'FlagData' result
--
-- Throws:
--
--    * An exception if any two flag names are duplicated in the input
--    @flag_data_list@.
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

-- | Constructs a 'FlagData' out of a 'GlobalRule'.
--
-- This global rule will be used in the last validation stage of the
-- 'process' method.
--
-- Usage example:
--
-- > flagData = combine [ flagToData user_id
-- >                     , validate (\fr -> if get fr user_id < 0
-- >                                       then Just "user id negative error"
-- >                                       else Nothing)
-- >                    ]
--
-- Multiple validation rules can exist.
--
-- Arguments:
--
--    *@global_rule@: global validation rule
--
-- Returns:
--
--    * A 'FlagData' representation of the @global_rule@.
validate :: GlobalRule -> FlagData
validate rule = (Map.empty, Map.empty, [rule])

-- | Converts a 'Flag' to a 'FlagData'.
--
-- 'FlagData' is the general form that is not bound by the type \"a\" of the
-- \"'Flag' a\" input, thus it can be added to a collection of flags later.
--
-- Flag is validaded with 'invalidFlag' and if an error is found on the flag
-- an exception is raised.
--
-- Arguments:
--
--    *@flag@: the flag to be mapped.
--
-- Returns:
--
--    * A corresponding 'FlagData' for the flag.
--
-- Throws:
--
--    * An exception if an error message is returned from the 'invalidFlag'
--    function.
flagToData :: Flag a -> FlagData
flagToData (Flag name help flagConf) = (flagData, aliasMap, [])
  where flagData = Map.singleton name (help, flagDConf)
        aliasMap = Map.fromList [(s, name) | s <- flagAlias flagConf]
        flagDConf = map fConfToFDataConf flagConf

fConfToFDataConf :: FlagConf a -> FlagDataConf
fConfToFDataConf conf = case conf of
    (FlagConf_DefaultIf _ p) -> FlagDataConf_HasDefault p
    (FlagConf_RequiredIf p) -> FlagDataConf_RequiredIf p
    (FlagConf_EmptyValueIs _) -> FlagDataConf_HasEmptyValue
    (FlagConf_Parser p) -> FlagDataConf_Validator (isJust . p)
    (FlagConf_Alias as) -> FlagDataConf_Alias as
    (FlagConf_DefaultOperation op) -> FlagDataConf_DefaultOperation op

invalidFlag :: (String, [FlagConf a]) -> Maybe String
invalidFlag (n, fc) = case invalidFlags of
    []    -> Nothing
    flags -> Just $ "Error: The following flags names are invalid "
                 ++ show flags
                 ++ ". A valid flag name consist of a letter followed "
                 ++ " by letters, numbers, dash or undercore."
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

mkDefaultOp :: [(String, (String, [FlagDataConf]))] -> DefaultOp
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

-- | Defines a flag.
--
-- A define flag consist of a name, a helptext and a list of flag
-- configurations.
--
-- The name is the flag identifier, it must be unique among other defined
-- flags and it must follow this pattern:
--
-- Arguments:
--
--    *@(name, helptext, configurations)@: A triple containing the flag name,
--    the helptext and the flag configurations.
--
-- Returns:
--
--    * A flag.
make :: (String, String, [FlagConf a]) -> Flag a
make (name, help, flagConf) = case anyErrorWithFlag of
                                  Nothing  -> Flag name help flagConf
                                  Just err -> error err
  where anyErrorWithFlag = listToMaybe $ catMaybes [validParser, validName]
        validParser = if null [True | (FlagConf_Parser _) <- flagConf]
                      then Just (flagError name "Flag parser was not provided")
                      else Nothing
        validName = invalidFlag (name, flagConf)

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
