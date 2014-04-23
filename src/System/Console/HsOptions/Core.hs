{- |
Module      :  System.Console.HsOptions.Core
Description :  Core HsOptions source
Copyright   :  (c) Jose Raymundo Cruz (jose.r.cruz01@gmail.com)
License     :  Apache-2.0

Maintainer  :  Jose Raymundo Cruz (jose.r.cruz01@gmail.com)
Stability   :  stable
Portability :  portable

Core HsOptions library source.
-}
module System.Console.HsOptions.Core
where

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
import System.Console.HsOptions.Types

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

-- | Creates a flag configuration that makes the flag as optional.
--
-- Since the flag is optional then it's type must
-- bet @'Flag' ('Maybe' a)@, so that it can be 'Nothing' if the flag was not
-- provided by the user or @'Just' value@ if it was provided.
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

-- | Creates a flag configuration that defines the default value when the flag
-- is empty.
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

-- | Creates a flag configuration that sets sets the default value for a flag
-- if the flag is not provided by the user.
--
-- Arguments:
--
--    * @default value@: the default value of the flag.
--
-- Returns:
--
--    * A flag configuration that sets the flag's default value.
--      This method is a specification of the 'defaultIs' method. Is is
--      equivalent to:
--
--      >>> defaultIf a (const True)
defaultIs :: a -> FlagConf a
defaultIs a = defaultIf (const $ Just a)

-- | Creates a flag configuration that sets a dependent default value for a
-- flag.
--
-- This configuration requires a function that takes in a `FlagResults` as
-- argument and returns a `Maybe a` value, `Nothing` says that there is no
-- default value and `Just something` says that there is a default value.
--
-- Arguments:
--
--    *@default_getter@: function that given a `FlagResults` returns the
--    default value (`Just`) or `Nothing` if no default value exist;
--
-- Returns:
--
--    * A flag configuration that sets the dependent default value for the
--    flag.
defaultIf :: (FlagResults -> Maybe a) -> FlagConf a
defaultIf = FlagConf_DefaultIf

-- | Creates a flag configuration for the aliases of the flag.
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

-- | Creates a flag configuration that marks a flag as conditionally required.
--
-- The flag will be required from the user if the @condition@ returns 'True'
-- and the user does not provides the flag in the input stream, a \"flag is
-- required\" error is displayed in this scenario.
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

-- | Sets the flag's parser configuration.
--
-- It takes the function that will parse the string input to the corresponding
-- flag type value. A set of this functions, such as 'intParser', was created
-- to provide a basic set of parsers.
--
-- Arguments:
--
--    *@parser_function@: a function that takes a flag argument and returns
--    'Nothing' if the argument can not be parsed or the parsed value ('Just')
--    if the argument can be parsed correctly.
--
-- Returns:
--
--    * A flag configuration that defines how to parse the string input.
parser :: (FlagArgument -> Maybe a) -> FlagConf a
parser = FlagConf_Parser

-- | Combination of the 'parser' and 'toMaybeParser' for syntactic sugar since
-- this is a very common scenario.
--
-- Basically is defined as:
--
-- > maybeParser = parser . toMaybeParser
--
-- So instead of always doing:
--
-- >>> user_id = make ("user_id", "help", [parser $ toMaybeParser intParser])
--
-- You can do:
--
-- >>> user_id = make ("user_id", "help", [maybePaser intParser])
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
maybeParser = parser . toMaybeParser

-- | Creates a flag configuration for the default operation of the flag.
--
-- Defines the default operation for the flag if no operation is made
-- explicit by the user. If this method is not called then the default
-- operation is always 'assign'.
--
-- >>> runhaskell Program.hs --user_name += batman
-- Operation was explicit: Operation = "Append operation"
--
-- >>> runhaskell Program.hs --user_name batman
-- Operation not specified: Operation = @default_operation@
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
-- This new function does will never fail if the argument is missing or
-- if the argument value is missing ('FlagMissing' or 'FlagValueMissing'),
-- instead this function maps any of these two to a 'Nothing' value.
--
-- If the flag argument is of type 'FlagValue' then the original parser
-- is used.
--
-- It is a convenient way to reuse current parsers, like 'intParser', without
-- having to redefine them. For instance \"'intParser'\" is of type
-- @'FlagArgument' -> ('Maybe' 'Int')@, but \"@toMaybeParser intParser@\" is of
-- type @'FlagArgument' -> ('Maybe' ('Maybe' 'Int'))@.
--
-- Usage example:
--
-- > user_id :: Flag (Maybe Int)
-- > user_id = make ("user_id", "help", [parser (toMaybeParser intParser)])
--
-- As you can observe @intParser@ was reused.
--
-- Arguments:
--
--    *@original_parser@: the original parser to be wrapped.
--
-- Returns:
--
--    * A new parser that changes the result type to an optional type.
toMaybeParser :: (FlagArgument -> Maybe a)
                   -> FlagArgument
                   -> Maybe (Maybe a)
toMaybeParser p arg = case arg of
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
--    *@flag_results@: the 'FlagResults' created from the input stream.
--
-- Returns:
--
--    * The value of the given flag.
--
-- Throws:
--
--    * An exception is raised if flag does not exist in the 'FlagResults'.
--    At this point, the 'get' method should always succeed parsing a flag
--    value. If the flag is not found then that means that the flag was not
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
flagDefault fr fc = fromMaybe Nothing result
  where result = listToMaybe [ p fr | (FlagConf_DefaultIf p) <- fc]

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
-- 'FlagData' is the general form of the flag that is not bounded by the
-- type @\"a\"@ of the @\"'Flag' a\"@ input, thus it can be added to a
-- collection of flags later.
--
-- Arguments:
--
--    *@flag@: the flag to be mapped.
--
-- Returns:
--
--    * A corresponding 'FlagData' for the flag.
flagToData :: Flag a -> FlagData
flagToData (Flag name help flagConf) = (flagData, aliasMap, [])
  where flagData = Map.singleton name (help, flagDConf)
        aliasMap = Map.fromList [(s, name) | s <- flagAlias flagConf]
        flagDConf = map fConfToFDataConf flagConf

-- | Maps a @'FlagConf' a@ to a 'FlagDataConf' data type.
--
-- Arguments:
--
--    *@flag_conf@: the flag configuration to be mapped to a 'FlagDataConf'.
--
-- Returns:
--
--    * A corresponding 'FlagDataConf' mapped from the input.
fConfToFDataConf :: FlagConf a -> FlagDataConf
fConfToFDataConf conf = case conf of
    (FlagConf_DefaultIf p) -> FlagDataConf_HasDefault (isJust . p)
    (FlagConf_RequiredIf p) -> FlagDataConf_RequiredIf p
    (FlagConf_EmptyValueIs _) -> FlagDataConf_HasEmptyValue
    (FlagConf_Parser p) -> FlagDataConf_Validator (isJust . p)
    (FlagConf_Alias as) -> FlagDataConf_Alias as
    (FlagConf_DefaultOperation op) -> FlagDataConf_DefaultOperation op

-- | Checks if a flag is invalid.
--
-- Among the validation rules checks that the file name follows the correct
-- convention, as well as all the flag\'s aliases.
--
-- Arguments:
--
--    *@(flag_name, flag_configuration)@: the data of the flag required to
--    validate it.
--
-- Returns:
--
--    * 'Nothing' if the flag is valid.
--
--    * An error message ('Just') if the flag failed.
invalidFlag :: (String, [FlagConf a]) -> Maybe String
invalidFlag (n, fc) = case invalidFlags of
    []    -> Nothing
    flags -> Just $ "Error: The following flags names are invalid "
                 ++ show flags
                 ++ ". A valid flag name consist of a letter followed "
                 ++ " by letters, numbers, dash or undercore."
  where invalidFlags = [x | x <- n:flagAlias fc, invalidFlagName x]

-- | Returns 'True' if the flag name is value, 'False' otherwise.
--
-- Enforces the convention for the flag name: \"@Letter@ followed by many @(
-- Letters - Numbers - Dashes (-) - Underscores (_) )@.
--
-- Arguments:
--
--    *@flag_name@: the name of the flag.
--
-- Returns:
--
--    * 'True' if the flag name follows the convention, 'False' otherwise.
invalidFlagName :: String -> Bool
invalidFlagName s = not (s =~ "^[a-zA-Z][a-zA-Z0-9\\-_]*$" :: Bool)

-- | Maps a flag alias to the real flag name.
--
-- For example if the flag @--user_id@ has the @-uid@ alias configured in the
-- @flag_alias_map@ then:
--
-- >>> mapAlias flag_alias_map "uid"
-- will return "user_id"
--
-- If the alias is not found in the @flag_alias_map@ then the @flag_alias@
-- argument is returned unmodified.
--
-- Arguments:
--
--    *@flag_alias_map@: the map of flag alias to flag name.
--
--    *@flag_alias@: the alias to be mapped to the real flag name.
--
-- Returns:
--
--    * The real flag name for the @flag_alias@ if found or the same argument
--    unmodified if not found.
mapAlias :: FlagAliasMap -> String -> String
mapAlias aliasMap name = fromMaybe name $ Map.lookup name aliasMap

-- | Replaces the @sub_string@ with the @replacement_string@ in the input
-- @input_string@.
--
-- >>> replaceStr "One two three@" ("two", "four")
-- "One four three"
--
-- Arguments:
--
--    *@input_string@: the string to be searched.
--
--    *@sub_string@: the string to be replaced in the @input_string@.
--
--    *@replacement_string@: the string to be used as a replacement of
--    @sub_string@.
--
-- Returns:
--
--    * The @input_string@ with any @sub_string@ replaced for
--    @replacement_string@.
replaceStr :: String -> (String, String) -> String
replaceStr str (pattern, repl) = subRegex (mkRegex pattern) str repl

-- | Creates the flag argument from the parser results for a given flag based
-- on the operation performed on the flag.
--
-- Maps the flag opereration to the respective method that will produce the
-- flag value, such as 'execAppend' (that produces the flag value by
-- appending the current value with the previous value).
--
-- Arguments:
--
--    *@parse_results@: the current state of the parse process.
--
--    *@(flag_name, operation, flag_value)@: flag and flag value information,
--    as well as the operation being performed to the flag.
--
-- Returns:
--
--    * A flag argument containing the final value for the flag after the
--    operation was performed.
executeOp :: ParseResults
          -> (String, OperationToken, FlagValueToken)
          -> FlagArgument
executeOp st (name, op, val) = case op of
    OperationTokenAssign   -> execAssign st (name, val)
    OperationTokenAppend   -> execAppend st (name, val)
    OperationTokenAppend'  -> execAppend' st (name, val)
    OperationTokenPrepend  -> execPrepend st (name, val)
    OperationTokenPrepend' -> execPrepend' st (name, val)

-- | Performs the assign flag operation.
--
-- Assigns the current value to the flag disregarding any previous state of
-- the flag.
--
-- Arguments:
--
--    *@parse_results@: the current state of the parse process.
--
--    *@(flag_name, flag_value)@: name and value of the current operation.
--
-- Returns:
--
--    * 'FlagValue' if the flag is being assigned to a not empty value,
--    'FlagValueMissing' otherwise.
execAssign :: ParseResults -> (String, FlagValueToken) -> FlagArgument
execAssign _ (name, val) = case val of
    FlagValueTokenEmpty    -> FlagValueMissing name
    (FlagValueToken value) -> FlagValue name value

-- | Performs the append flag operation.
--
-- Appends the current value of the flag to the previous value of the flag.
-- A space is used as a separator between the two values.
--
-- If the current value for the flag is empty then the previous value is
-- taken as the new flag value.
--
-- Arguments:
--
--    *@parse_results@: the current state of the parse process.
--
--    *@(flag_name, flag_value)@: name and value of the current operation.
--
-- Returns:
--
--    * The current value of the flag appended to the previous value of the
--    flag using a space as separator.
execAppend :: ParseResults -> (String, FlagValueToken) -> FlagArgument
execAppend (fr, _) (name, val) = case val of
    FlagValueTokenEmpty    -> previousOrEmpty fr name
    (FlagValueToken value) -> FlagValue name (inheritKw ++ prefix ++ value)
  where prefix =  if isJust $ Map.lookup name fr then " " else ""

-- | Performs the append' flag operation.
--
-- Similar to 'append' but does not use a space as separator.
--
-- If the current value for the flag is empty then the previous value is
-- taken as the new flag value.
--
-- Arguments:
--
--    *@parse_results@: the current state of the parse process.
--
--    *@(flag_name, flag_value)@: name and value of the current operation.
--
-- Returns:
--
--    * The current value of the flag appended to the previous value of the
--    flag without using any in between separator.
execAppend' :: ParseResults -> (String, FlagValueToken) -> FlagArgument
execAppend' (fr, _) (name, val) = case val of
    FlagValueTokenEmpty    -> previousOrEmpty fr name
    (FlagValueToken value) -> FlagValue name (inheritKw ++ value)

-- | Performs the prepend flag operation.
--
-- Prepends the current value of the flag to the previous value of the flag.
-- A space is used as a separator between the two values.
--
-- If the current value for the flag is empty then the previous value is
-- taken as the new flag value.
--
-- Arguments:
--
--    *@parse_results@: the current state of the parse process.
--
--    *@(flag_name, flag_value)@: name and value of the current operation.
--
-- Returns:
--
--    * The current value of the flag prepended to the previous value of the
--    flag using a space as separator.
execPrepend :: ParseResults -> (String, FlagValueToken) -> FlagArgument
execPrepend (fr, _) (name, val) = case val of
    FlagValueTokenEmpty    -> previousOrEmpty fr name
    (FlagValueToken value) -> FlagValue name (value ++ prefix ++ inheritKw)
  where prefix =  if isJust $ Map.lookup name fr then " " else ""

-- | Performs the prepend' flag operation.
--
-- Similar to 'prepend' but does not use a space as separator.
--
-- If the current value for the flag is empty then the previous value is
-- taken as the new flag value.
--
-- Arguments:
--
--    *@parse_results@: the current state of the parse process.
--
--    *@(flag_name, flag_value)@: name and value of the current operation.
--
-- Returns:
--
--    * The current value of the flag prepend to the previous value of the
--    flag without using any in between separator.
execPrepend' :: ParseResults -> (String, FlagValueToken) -> FlagArgument
execPrepend' (fr, _) (name, val) = case val of
    FlagValueTokenEmpty    -> previousOrEmpty fr name
    (FlagValueToken value) -> FlagValue name (value ++ inheritKw)

-- | Tries to find the previous result of a flag in the 'FlagResults'.
--
-- Arguments:
--
--    *@parse_results@: the current parsed flags.
--
--    *@flag_name@: flag name used to find the flag\'s value.
--
-- Returns:
--
--    * The value of the flag inside the @parse_results@ if it exists or
--    'FlagValueMissing' otherwise.
previousOrEmpty :: FlagResults -> String -> FlagArgument
previousOrEmpty fr name = fromMaybe (FlagValueMissing name)
                                    (Map.lookup name fr)

-- | Checks if the value for the flag includes the $(inherit) keyword and
-- replaces this keyword by the previous value of the flag.
--
-- It is a generalization of the append or prepend features, you can insert
-- the previous value of the flag in any position of the new value.
--
-- For example, if the previous flag value was @\"batman\"@ and the new value
-- was @\"a-$(inherit)-z\"@ then the new value will be expanded to
-- @\"a-batman-z\"@.
--
-- Arguments:
--
--    *@parse_results@: the current parsed flags.
--
--    *@argument@: the value of the flag.
--
-- Returns:
--
--    * A new flag value by expanding any $(inherit) keyword with the previous
--    value of the flag.
applyInherit :: ParseResults -> FlagArgument -> FlagArgument
applyInherit (fr, _) arg = case arg of
    (FlagMissing _)         -> arg
    (FlagValueMissing _)    -> arg
    (FlagValue name value0) -> FlagValue name value1
      where value1 = value0 `replaceStr` (inheritRegex, previous)
            previous = case Map.lookup name fr of
                           Just (FlagValue _ v) -> v
                           _                    -> ""

-- | Takes a 'Token' and produces a 'ParseResults'.
--
-- 'ParseResults' contains the final result of the flag after being analysed.
--
-- If the token is an 'ArgToken' then it is mapped unmodified to a
-- 'ParseResults'. If the token is a 'FlagToken' then the flag operation is
-- performed and the inherit keyword is expanded, then a 'ParseResults' is
-- created with this final flag value.
--
-- Arguments:
--
--    *@(parse_results, token)@: the parse results so far and the current
--    token to parse.
--
-- Returns:
--
--    * A parse results after evaluating the current token.
parseToken :: (ParseResults, Token) -> ParseResults
parseToken (st, tok) = case tok of
    (ArgToken arg)             -> (Map.empty, [arg])
    (FlagToken name op value0) -> let value1 = executeOp st (name, op, value0)
                                      value2 = applyInherit st value1
                                  in (Map.singleton name value2, [])

-- | Takes a list of tokens, an initial 'ParseResults' and returns a
-- complete merge 'ParsedResults' after parsing each token in order.
--
-- Every token is parsed using the 'parseToken' and it\'s result is
-- continuously merged until all tokens are consumed.
--
-- Arguments:
--
--    *@tokens@: the list of tokens to parse.
--
--    *@initial_parse_result@: the initial state of the parse process.
--
-- Returns:
--
--    * A complete 'ParseResults' after parsing all of the input tokens in
--    order.
parseArgs :: [Token] -> ParseResults -> ParseResults
parseArgs [] st0 = st0
parseArgs (tok:toks) st0 = let st1 = parseToken (st0, tok)
                               st2 = mergeSt (st0, st1)
                           in parseArgs toks st2
  where mergeSt ((fr1, args1), (fr2, args2)) = ( fr2 `Map.union` fr1
                                               , args1 ++ args2)

-- | Concatenates two 'TokenizeResult' together.
--
-- If both are errors ('Left') then the errors are concatenated.
--
-- If only one of them is errors ('Left') then that result is returned.
--
-- If non are errors, but tokens ('Right'), then the tokens are concatenated
-- and returned.
--
-- Arguments:
--
--    *@tokenize_result1@: first result.
--
--    *@tokenize_result2@: second result.
--
-- Returns:
--
--    * @tokenize_result1@ concatenated with @tokenize_result2@
concatToks :: TokenizeResult -> TokenizeResult -> TokenizeResult
concatToks (Left errs) (Left errs2) = Left (errs ++ errs2)
concatToks (Left errs) _ = Left errs
concatToks _ (Left errs) = Left errs
concatToks (Right toks1) (Right toks2) = Right $ toks1 ++ toks2

-- | Reads a file from disk and returns the file content or an error.
--
-- Arguments:
--
--    *@filename@: the filename of the file to be opened.
--
-- Returns:
--
--    * Either an exception ('Left') or the file content ('Right').
readFile' :: String -> IO (Either SomeException String)
readFile' name = try $ readFile name :: IO (Either SomeException String)

-- | Parses the content of a configuration file.
--
-- If there is an error reading the file then that error is returned,
-- otherwise the file content is processed and tokenized.
--
-- A preprocessor stage is performed on the content to remove the comments
-- in the text file (comments starts with a hash (#) ).
--
-- Arguments:
--
--    *@(parents, flag_data)@: the parents is the tree of configuraiton file
--    parents of this current configuration file. The flag_data is the
--    information about the flags defined in the code.
--
--    *@configuration_filename@: the path of the configuration file to include.
--
-- Returns:
--
--    * The tokenized version of the content of the configuration file or
--    an error ('Left') if any errors found during the process.
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

-- | Removes all the comments of the @input_text@.
--
-- A comment is a string that begins with hash symbol (#) and ends with a
-- line change (\'\\n\').
--
-- Arguments:
--
--    *@input_text@: the input text.
--
-- Returns:
--
--    * The same input text but with all comments removed.
removeComments :: String -> String
removeComments []     = []
removeComments (c:cs) = if c == '#'
                        then removeComments $ dropWhile (/= '\n') cs
                        else c:removeComments cs

-- | Checks if the token is the flag @\'--usingFile\'@.
--
-- Arguments:
--
--    *@token@: the token to evaluate.
--
-- Returns:
--
--    * 'Nothing' if the token is not for the @--usingFile@ flag.
--
--    * The canonical path of the configuration file ('Just') if the token is
--    for the flag @--usingFile@.
isUsingConfFlag :: Token -> IO (Maybe String)
isUsingConfFlag (FlagToken name _ (FlagValueToken file)) =
    if name `notElem` uncurry (:) usingFileKw
    then return Nothing
    else liftM Just (canonicalizePath file)
isUsingConfFlag _ =  return Nothing

-- | Recursively expands all the configuration file includes from the
-- input token stream.
--
-- It takes the input tokens and searches for any instance of the
-- @--usingFile@ flag. If this flag is found then that flag is consumed,
-- the configuration file defined by that flag is tokenized and these new
-- tokens are inserted in the current spot of the input token stream.
--
-- This is a recurvise process that will stop once all the @--usingFile@
-- have been consumed and we got a stream of pure flag tokens.
--
-- If a circular dependency is detected then an error is reported to user.
-- @parents@ has the current tree of parents so we can check if we are
-- including one parent configuration file, which will cause an infinite loop.
--
-- Arguments:
--
--    *@(parents, flag_data)@: the parents is the tree of configuraiton file
--    parents of this current configuration file. The flag_data is the
--    information about the flags defined in the code.
--
--    *@input_tokens@: the input token stream.
--
-- Returns:
--
--    * A new token stream with all the configuration file includes expanded,
--    or an error if any error is found (either with circular dependency or
--    invalid flag values).
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

-- | Creates an error message when a circular dependency is found.
--
-- Creates a pretty error highlighting the circular dependency.
--
-- Arguments:
--
--    *@tree@: the tree of configuration files that caused the problem.
--
-- Returns:
--
--    * An error showing the problem.
reportCircularDependency :: [String] -> IO TokenizeResult
reportCircularDependency files = return (Left [FlagFatalError msg])
  where msg = "Error while parsing conf file: "
           ++ "Circular includes on files\n"
           ++ format files
        format fs = case fs of
                        []     -> ""
                        [x]    -> "  " ++ x
                        (x:xs) -> "  " ++ x ++ " ->\n" ++ format xs

-- | Takes an input string and creates a stream of tokens after parsing
-- the input with the 'parseInput'.
--
-- After the initial token stream is created all the configuration files
-- includes are expanded to create a clean token stream.
--
-- Arguments:
--
--    *@(parents, flag_data)@: the parents is the tree of configuraiton file
--    parents of this current configuration file. The flag_data is the
--    information about the flags defined in the code.
--
--    *@input_string@: the input string to tokenize.
--
--
--  Returns:
--
--      * A tokenized stream processed from the input string.
tokenize :: ([String], FlagData) -> String -> IO TokenizeResult
tokenize (parents, flags) input = includeConfig (parents, flags) toks
  where (fd, _, _) = flags
        defaultOp = mkDefaultOp $ Map.toList fd
        toks = parseInput defaultOp input

-- | Creates a map of flag name to flag default operation.
--
-- This map is used in the parser to correctly retrieve which is
-- the default operation for each flag when the user does not explicitly
-- specifies the flag operation.
--
-- Arguments:
--
--    *@flag_list@: all the flags in the code (including the flag
--    configuration).
--
-- Returns:
--
--    * A map from flag to default operation.
mkDefaultOp :: [(String, (String, [FlagDataConf]))] -> DefaultOp
mkDefaultOp [] = Map.empty
mkDefaultOp (x:xs) = Map.singleton name defaultOp `Map.union` defaultOps
  where (name, (_, flagDataConf)) = x
        defaultOps = mkDefaultOp xs
        defaultOp = flagDDefaultOperation flagDataConf

-- | Does the actual flag parsing.
--
-- It begins by parsing the entire input stream and creating a tokenized
-- input stream. A recursive search is done in these tokens to find any
-- configuration files includes. Each conf file include is expanded until
-- the tokenized stream is just composed of flags and positional arguments.
--
-- This tokenized stream is validated then by the rules of each flag, such as
-- type validation, required flags, constraints, etc.
--
-- This method is an IO action because it needs to expand the configuration
-- file includes (open the file and get it\'s content). When the stream is
-- tokenized and no more IO is needed the 'process\'' method is called.
--
-- The validation is divided in two, local validation and global validation.
--
-- Local validation handled things like reserved words, unknown flags and
-- correct flag types.
--
-- Global validation happens at the end and does all the validation that needs
-- a context, such as global validation constraints, conditionally required
-- flags and dependent default. Global validation only happens if the local
-- validation succeeds.
--
-- Arguments:
--
--    *@flag_data@: a set of all the flags to process.
--
--    *@input_stream@: the command line input stream.
--
-- Returns:
--
--    * Either a @list of errors@ ('Left') or a successful @result@ ('Right').
process :: FlagData -> [String] -> IO (Either [FlagError] ProcessResults)
process fd args = do result <- tokenize ([], fd) (unwords args)
                     case result of
                         Left errs -> return $ Left errs
                         Right toks -> return $ process' fd toks

-- | Does the actual flag parsing after the input stream have been tokenized
-- and the configuration files have been expanded.
--
-- Updates the tokenized input stream and changes any flag alias set by the
-- user to the actual name of the flag, meaning if the user sent @--uid = 8@
-- then the token is changed to @--user_id = 8@ (considering @uid@ is an alias
-- for @user_id@). This means from this point forward the flag name is used
-- as the flag identifier (any alias was mapped to the flag name).
--
-- Performs local validation:
--
--    * Verifies that no reserved words was used for the flag\'s name.
--
--    * Validates that no unknown flag was sent by the user.
--
--    * Validates that for every value set to a flag this value can be parsed
--    to the type the flag is expecting. (i.e. checks the string value of an
--    'Int' flag can be parsed from string to 'Int' correctly).
--
-- Performs global validation:
--
--    * Validates the 'requiredIf' constraints.
--
--    * Validates the 'defaultIf' constraints.
--
--    * Validates all global validation rules are passing (rules created by
--    using the 'validate' function)
--
-- Arguments:
--
--    *@flag_data@: a set of all the flags to process.
--
--    *@tokenized_input_stream@: the complete input stream after tokenized.
--
-- Returns:
--    * Either a @list of errors@ ('Left') or a successful @result@ ('Right').
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

-- | Maps the flag alias to the real flag name for each of the input tokens.
--
-- The alias is mapped using the 'mapAlias' method.
--
-- Arguments:
--
--    *@flag_data@: contains information for the configured aliases.
--
--    *@tokens@: the list of tokens.
--
-- Returns:
--
--    * An update list of tokens with each flag alias replaced with the
--    real name of the flag.
updateFlagAlias :: FlagData -> [Token] -> [Token]
updateFlagAlias (_, aliasMap, _) = map updateAlias
  where updateAlias tok = case getFlagName tok of
                              Nothing   ->  tok
                              Just name -> let name' = mapAlias aliasMap name
                                           in updateName tok name'

-- | Gets the flag name from a token if the token is a flag.
--
-- Arguments:
--
--    *@token@: the token.
--
-- Returns:
--
--    * The flag name ('Just') if the token is a 'FlagToken'. 'Nothing'
--    otherwise.
getFlagName :: Token -> Maybe String
getFlagName tok = case tok of
                      (FlagToken name _ _) -> Just name
                      _                    -> Nothing

-- | Replaces the name of a flag with a new name.
--
-- Arguments:
--
--    *@token@: the token.
--
--    *@name@: the new name for the flag.
--
-- Returns:
--
--    * An updated token with the name replaced with the new name if the token
--    is a 'FlagToken'. The unmodified token otherwise.
updateName :: Token -> String -> Token
updateName tok name = case tok of
                          (FlagToken _ op value) -> FlagToken name op value
                          _                      -> tok

-- | Checks if the help flag was sent by the user.
--
-- Arguments:
--
--    *@arguments@: the arguments from the user.
--
-- Returns:
--
--    * 'True' if the user sent the @--help@ or the @-h@ flags.
anyArgIsHelp :: [String] -> Bool
anyArgIsHelp args = any (`elem` args) helpFlags
  where helpFlags = concat [["--" ++ x, "-" ++ x] | x <- uncurry (:) helpKw]

-- | Processes the input arguments and parses all the flags.
--
-- Starts the process flags pipeline. First checks if the user wants to
-- display the help text of the program. This is done by looking for the
-- flag \"@--help@\" or \"@-h@\" flag in the input stream. If this flag is
-- found then the @display_help@ function is called with all the compiled
-- helptext and the program description.
--
-- If the user does not wants to display the help then the 'process' function
-- is called to do the actual flag parsing. This function can return errors
-- or a success result ('FlagResults', 'ArgsResults'). If any error is found
-- then the @failure@ function is called with the list of errors, otherwise the
-- @success@ function is called with the results.
--
-- See the 'process' documentation to see the rules and conditions of flag
-- parsing.
--
-- Arguments:
--
--    *@description@: the description of the program. Sent to the
--    @display_help@ function.
--
--    *@flag_data@: a collection of all the flags defined in the code.
--
--    *@success@: a success callback function. Called if no errors were found
--    while parsing the flags from the input.
--
--    *@failure@: a failure callback function. Called if some errors were
--    found while parsing the input.
--
--    *@display_help@: a display help callback function. Called when the
--    \"@--help@\" or \"@-h@\" flag was sent by the user.
--
-- Returns:
--
--    * An IO action.
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

-- | Returns the arguments sent by the user.
--
-- Double quote any argument that contains whitespace.
--
-- Returns:
--
--    * The list of the command line arguments quoting any argument with
--    whitespace.
getQuotedArgs  :: IO [String]
getQuotedArgs = do args <- getArgs
                   return $ map quote' args
  where quote' s = if length (words s) > 1
                   then "\"" ++ s ++ "\""
                   else s

-- | Checks if a fatal error occurred.
--
-- Arguments:
--
--    *@errors@: all flag errors.
--
-- Returns:
--
--    * 'True' if a 'FlagFatalError' is found. 'False' otherwise.
hasFatalError :: [FlagError] -> Bool
hasFatalError errs = not $ null [True | FlagFatalError _ <- errs]

-- | Merges a list of pipelines.
--
-- If any pipeline returns an error then the error is returned without
-- processing the remaining pipelines.
--
-- Arguments:
--
--    *@pipelines@: a list of pipelines.
--
-- Returns:
--
--    * A pipeline function that merges a list of pipelines.
pipelines :: [[PipelineFunction]] -> PipelineFunction
pipelines [] (_, flags)      = ([], flags)
pipelines (p:ps) (fd, flags) = case pipeline p (fd, flags) of
                                   ([], res) -> pipelines ps (fd, res)
                                   errs      -> errs

-- | Executes the pipeline functions in order.
--
-- The result of the current pipeline function is sent to the next function
-- in the list.
--
-- The functions are evaluated until the list is consumed or any function
-- returns a fatal error.
--
-- Arguments:
--
--    *@pipelines@: the list of pipeline functions
--
-- Returns:
--
--    * A pipeline function that executes the list of pipelines in order.
pipeline :: [PipelineFunction] -> PipelineFunction
pipeline [] (_, flags) = ([], flags)
pipeline (val:vs) (fd, flags0) = case val (fd, flags0) of
    ([], flags1)    -> pipeline vs (fd, flags1)
    (errs1, flags1) -> if hasFatalError errs1
                       then (errs1, flags1)
                       else let (errs2, flags2) = pipeline vs (fd, flags1)
                            in (errs1 ++ errs2, flags2)

-- | Pipeline function that validates no flag is using a reserved word as
-- it\'s name.
--
-- Arguments:
--
--    *@(flag_data, flag_results)@: the flag data and the current flag
--    results.
--
-- Returns:
--
--    * An error if any flag is using a reserved word as it\'s name. The
--    @flag_results@ is not modified for the next pipeline function.
validateReservedWords :: PipelineFunction
validateReservedWords ((fd, aliasMap, _), flags) =
    case reservedWords `intersect` codeFlags of
        []    -> ([], flags)
        names -> (map reservedWordsError names, flags)
  where codeFlags = Map.keys fd ++ Map.keys aliasMap
        errorMsg = "The name is a reserved word and can not be used"
        reservedWordsError name = FlagFatalError $ flagError name errorMsg

-- | Applies the predicate if the value is 'Just'.
--
-- Arguments:
--
--    *@value@: value to apply the predicate
--
--    *@predicate@: the predicate to apply.
--
-- Returns:
--
--    * 'True' if the value is 'Nothing'.
--
--    * The predicate applied to the internal value if is 'Just'.
ifIsJust :: Maybe a -> (a -> Bool) -> Bool
ifIsJust val predicate = case val of
                            Nothing -> True
                            Just a  -> predicate a

-- | Pipeline function that modifies the results by inserting all missing
-- flags into it.
--
-- At this point the flags have been processed by the parser, so the
-- 'FlagResults' will have only the flags sent by the user. The remaining
-- flags are inserted into the 'FlagResults' with a 'FlagMissing' value.
--
-- This is useful so that future pipeline functions don\'t need to worry
-- about any flag not present in the flag results, as all defined flags
-- will be there, and missing flags will be there with the 'FlagMissing'
-- value.
--
-- Arguments:
--
--    *@(flag_data, flag_results)@: the flag data and the current flag
--    results.
--
-- Returns:
--
--    * The modified @flag_results@.
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

-- | Pipeline function that checks if any unknown flag was sent by the user.
--
-- If a flag exists in the @flag_results@ that does not exist in the
-- @flag_data@ then an error is returned.
--
-- Arguments:
--
--    *@(flag_data, flag_results)@: the flag data and the current flag
--    results.
--
-- Returns:
--
--    * An error if an unknown flag is found.
validateUnknownFlags :: PipelineFunction
validateUnknownFlags ((fd, aliasMap, _), flags) = (errors, flags)
  where inputFlags = Map.keys flags
        codeFlags = Map.keys fd ++ Map.keys aliasMap
        missingFlags = inputFlags \\ codeFlags
        errors = map (FlagFatalError . flagUnkownError) missingFlags
        errorMsg = "Unkown flag is not defined in the code"
        flagUnkownError name = flagError name errorMsg

-- | Pipeline function that runs the 'checkValidator'  on all the
-- define flags on @flag_data@.
--
-- Arguments:
--
--    *@(flag_data, flag_results)@: the flag data and the current flag
--    results.
--
-- Returns:
--
--    * An error if the flag validator fails.
validateFlagParsers :: PipelineFunction
validateFlagParsers ((fd, _, _), flags) =
    (mapMaybe aux (Map.toList fd), flags)
  where aux (name, (_, conf)) = case checkValidator conf value of
            ValidationError err -> Just err
            _                   -> Nothing
          where value = fromJust (Map.lookup name flags)

-- | Pipeline function that runs the 'requiredIfValidator'  on all the
-- define flags on @flag_data@.
--
-- Arguments:
--
--    *@(flag_data, flag_results)@: the flag data and the current flag
--    results.
--
-- Returns:
--    * An error if the flag is required.
validateRequiredIf :: PipelineFunction
validateRequiredIf ((fd, _, _), flags) = (mapMaybe aux (Map.toList fd), flags)
  where aux (name, (_, flagDataConf)) =
            case requiredIfValidator flagDataConf flags value of
                ValidationError err -> Just err
                _                   -> Nothing
          where value = fromJust (Map.lookup name flags)

-- | Pipeline function that runs the 'defaultIfValidator'  on all the
-- define flags on @flag_data@.
--
-- Arguments:
--
--    *@(flag_data, flag_results)@: the flag data and the current flag
--    results.
--
-- Returns:
--    * An error if the flag is required.
validateDependentDefault :: PipelineFunction
validateDependentDefault ((fd, _, _), flags) =
    (mapMaybe aux (Map.toList fd), flags)
  where aux (name, (_, flagDataConf)) =
            case defaultIfValidator flagDataConf flags value of
                ValidationError err -> Just err
                _                   -> Nothing
          where value = fromJust (Map.lookup name flags)

-- | Pipeline function that runs all the global rules defined in the
-- @flag_data@.
--
-- Arguments:
--
--    *@(flag_data, flag_results)@: the flag data and the current flag
--    results.
--
-- Returns:
--    * An error if any validation rules fails.
validateGlobalRules :: PipelineFunction
validateGlobalRules ((_, _, gr), flags) = (flagErrs, flags)
  where errs = mapMaybe (\ r -> r flags) gr
        flagErrs = map FlagNonFatalError errs

-- | Validates if the flag should be conditionally required.
--
-- If the flag is missing, was marked as conditionally required (using
-- 'requiredIf') and the required predicate returns true then
-- a \"flag required\" error is reported.
--
-- In any other condition a success validation result is returned.
--
-- Arguments:
--
--    *@flag_configurations@: the configurations for a flag.
--
--    *@flag_results@: current state of the flags.
--
--    *@flag_argument@: the flag argument.
--
-- Returns:
--
--    * 'ValidationSuccess' if the flag is not required.
--    An error ('ValidationError') if it is not.
requiredIfValidator :: [FlagDataConf]
                    -> FlagResults
                    -> FlagArgument
                    -> ValidationResult
requiredIfValidator fdc fr (FlagMissing name)
  | flagDIsRequiredIf fdc fr = validationError name "Flag is required"
  | otherwise = ValidationSuccess
requiredIfValidator _ _ _ = ValidationSuccess

-- | Validates if the flag has a default value or the flag is optional
-- when the flag is missing.
--
-- If the flag is missing and it is not optional or it does not have a
-- default value configured then a \"flag required\" error is reported.
--
-- In any other condition a success validation result is returned.
--
-- Arguments:
--
--    *@flag_configurations@: the configurations for a flag.
--
--    *@flag_results@: current state of the flags.
--
-- Returns:
--
--    * 'ValidationSuccess' if the flag is valid with respect to the default
--    value. An error ('ValidationError') if it is not.
defaultIfValidator :: [FlagDataConf]
                   -> FlagResults
                   -> FlagArgument
                   -> ValidationResult
defaultIfValidator fdc fr (FlagMissing name)
  | flagDHasDefaultForResults fdc fr = ValidationSuccess
  | flagDIsOptional fdc  = ValidationSuccess
  | otherwise = validationError name "Flag is required"
defaultIfValidator _ _ _ = ValidationSuccess

-- | Defines a flag.
--
-- A defined flag consist of a name, a helptext and a list of flag
-- configurations.  The name is the flag identifier, it must be unique among
-- all other defined flags.
--
-- The name must follow the pattern \" @\'Letter\'@ followed by many
-- @\'Letters || Numbers || Dashes (-) || Underscores (_)\'@ \".
-- If the name of the flag is invalid an
-- exception is thrown.
--
-- A parser for the flag must be set in the @configuration@ by using 'parser'
-- or 'maybeParser'. If a parser is not found an exception is thrown.
--
-- Arguments:
--
--    *@(name, helptext, configurations)@: A triple containing the flag name,
--    the helptext and the flag configurations.
--
-- Returns:
--
--    * A flag.
--
-- Throws:
--
--    * An exception if the name is invalid (does not follows the pattern).
--
--    * An exception if the parser was not set in the @configurations@.
make :: (String, String, [FlagConf a]) -> Flag a
make (name, help, flagConf) = case anyErrorWithFlag of
                                  Nothing  -> Flag name help flagConf
                                  Just err -> error err
  where anyErrorWithFlag = listToMaybe $ catMaybes [validParser, validName]
        validParser = if null [True | (FlagConf_Parser _) <- flagConf]
                      then Just (flagError name "Flag parser was not provided")
                      else Nothing
        validName = invalidFlag (name, flagConf)

-- | Prints the help text to the screen using an standard format.
--
-- Arguments:
--
--    *@description@: the description of the program.
--
--    *@flag_helptexts@: A list of triples (name, aliases, helptext) that
--    contains an entry for each flag, with the flag's name, aliases and
--    helptext.
defaultDisplayHelp :: String -> [(String, [String], String)] -> IO ()
defaultDisplayHelp desc flags = putStrLn helpText
  where helpText = Opt.usageInfo desc (map getOptDescr flags)
        getOptDescr (name, alias, help) = mapOptOption (name:alias) help

-- | Creates an option description from the names and helptext.
--
-- Arguments:
--
--    *@names@: the names for the flag.
--
--    *@helptext@: the helptext for the flag.
--
-- Returns:
--
--    * An 'Opt.OptDescr' that describes the flag.
mapOptOption :: [String] -> String -> Opt.OptDescr String
mapOptOption names = Opt.Option short long (Opt.NoArg "")
  where (short, long) = splitShortName names

-- | Takes the single character strings from the @input_list@ and
-- separates them from the list.
--
-- >>> splitShortName ["one", "a", "two","b"]
-- ("ab", ["one", "two"])
--
-- Arguments:
--
--    *@input_list@: a list of strings.
--
-- Returns:
--
--    * A tuple where the first element is a list of the single character
--    strings from the @input_list@ and the second if the remaining strings
--    from the @input_list@.
splitShortName :: [String] -> (String, [String])
splitShortName = foldl aux ([], [])
  where aux (s, l) current = if length current == 1
                             then (s ++ [head current], l)
                             else (s, l ++ [current])

-- | Displaye the list of errors to the screen.
--
-- Arguments:
--
--    *@errors@: list of errors to display.
defaultDisplayErrors :: [FlagError] -> IO ()
defaultDisplayErrors errs = do putStrLn "Errors occurred while parsing flags:"
                               mapM_ print errs

-- | Creates a data structure with all the flag helptext information.
--
-- Arguments:
--
--    *@flag_data@: all configured flags in the system.
--
-- Returns:
--
--    * A list of triples. Each triple contains (flag_name, [flag_alias],
--    flag_helptext). After all flags in @flag_data@ are processed the
--    two system flags (@help@ and @usingFile@) are appended to the end of
--    the list.
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

-- | Returns whether the flag configuration marked this flag as optional.
--
-- Arguments:
--
--    *@flag_configurations@: the configurations for a flag.
--
-- Returns:
--
--    * 'True' if the flag is an optional flag. 'False' otherwise.
flagDIsOptional :: [FlagDataConf] -> Bool
flagDIsOptional fdc = not $ null [True | (FlagDataConf_RequiredIf _) <- fdc]

-- | Returns whether the flag should be required to the user based on the
-- current results.
--
-- First checks if the flag was configured as conditionally required (with the
-- use of the 'requiredIf' function).
--
-- If it was, this required condition is checked against the current
-- state.
--
-- Arguments:
--
--    *@flag_configurations@: the configurations for a flag.
--
--    *@flag_results@: current state of the flags.
--
-- Returns:
--
--    * 'True' if the flag should be required to the user.
flagDIsRequiredIf :: [FlagDataConf] -> FlagResults -> Bool
flagDIsRequiredIf fdc fr = case res of
                              Nothing -> False
                              Just p  -> p fr
   where res = listToMaybe [ p | (FlagDataConf_RequiredIf p) <- fdc]

-- | Returns `True` if any of the `FlagDataConf_HasDefault` returns
-- `True`.
--
-- For all the configured default values of the flag (like when
-- using `defaultIs` or `defaultIf` checks if any of the predicates
-- returns true for the given `FlagResults`.
--
-- Arguments:
--
--    *@flag_configurations@: the configurations for a flag.
--
--    *@flag_results@: current state of the flags.
--
-- Returns:
--
--    * 'True' if there is any default value configured for the flag.
--    'False' otherwise.
flagDHasDefaultForResults :: [FlagDataConf] -> FlagResults -> Bool
flagDHasDefaultForResults fdc fr = result
   where result = or [ p fr | (FlagDataConf_HasDefault p) <- fdc]

-- | Checks if a dependent default value was configured for the flag, like
-- when the 'defaultIf' or 'defaultIs' functions are used.
--
-- Arguments:
--
--    *@flag_configurations@: the configurations for a flag.
--
-- Returns:
--
--    * 'True' if a dependent default was configured for the flag.
--    'False' otherwise.
flagDHasDefault :: [FlagDataConf] -> Bool
flagDHasDefault fdc = not $ null [ True | (FlagDataConf_HasDefault _) <- fdc]

-- | Checks if an empty value was configured for the flag, like
-- when the 'emptyValueIs' function is used.
--
-- Arguments:
--
--    *@flag_configurations@: the configurations for a flag.
--
-- Returns:
--
--    * 'True' if an empty value was configured for the flag.
--    'False' otherwise.
flagDHasEmptyValue :: [FlagDataConf] -> Bool
flagDHasEmptyValue fdc = not $ null [ True | FlagDataConf_HasEmptyValue <- fdc]

-- | Finds the flag validator ('FlagDataConf_Validator') in the list of
-- flag configurations and runs it against the flag argument.
--
-- Arguments:
--
--    *@flag_configurations@: the configurations for a flag.
--
--    *@flag_argument@: the argument for the flag.
--
-- Returns:
--
--    * The result of the validator when run against the flag argument.
runDValidator :: [FlagDataConf] -> FlagArgument -> Bool
runDValidator fdc = validator
  where validator = head [x | (FlagDataConf_Validator x) <- fdc]

-- | Creates a failed validation result for a flag.
--
-- Constructor method to easily created a failed validation result
-- ('ValidationError').
--
-- Arguments:
--
--    *@flag_name@: flag name that caused the error.
--
--    *@error_message@: error message.
--
-- Returns:
--
--    * A validation result for a flag error.
validationError :: String -> String -> ValidationResult
validationError name s = ValidationError $ FlagNonFatalError errorMsg
  where errorMsg = flagError name s

-- | Checks if the flag value provided in 'FlagArgument' is valid by using
-- the validation found in the @flag_configurations@.
--
-- The configured flag validator will only be used if required.
--
-- Under these conditions the flag is considered valid, and a
-- 'ValidationSuccess' is returned, without the need to use the flag
-- validator:
--
--    * The flag is missing but was marked as optional.
--
--    * The flag is missing but has a default value.
--
--    * The flag is empty but has an empty value configured.
--
-- If none of the above applies, then this is the expected results:
--
--    * The flag is missing: A \"flag is required\" error is reported.
--
--    * The flag is empty: A \"flag value is empty\" error is reported.
--
--    * The flag has a value: The configured flag validator is run with this
--    value. If it fails a \"flag value invalid\" error is reported.
--
-- Arguments:
--
--    *@flag_configurations@: the configurations for a flag.
--
--    *@flag_argument@: the argument for the flag.
--
-- Returns:
--
--    * 'ValidationSuccess' if the flag is valid or an error (
--    'ValidationError') if it is not.
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

-- | Helper parser function.
--
-- Parses any type that is instance of the 'Read' class.
--
-- Arguments:
--
--    *@argument@: the input argument.
--
-- Returns:
--
--    * 'Nothing' if the argument is missing or if it's value is missing.
--
--    * Otherwise it calls 'read' on the argument value and returns a \"
--    'Maybe' a\" with the result of read.
valueParser :: Read a => FlagArgument -> Maybe a
valueParser arg = case arg of
                      (FlagMissing _)      -> Nothing
                      (FlagValueMissing _) -> Nothing
                      (FlagValue _ value)  -> readMaybe value

-- | Parses an 'Int' from the input 'FlagArgument'
--
-- Arguments:
--
--    *@argument@: the input argument
--
-- Returns:
--
--    * 'Nothing' if the argument cannot be parsed to the type or the @parsed
--    value ('Just')@ if it can.
intParser :: FlagArgument -> Maybe Int
intParser = valueParser

-- | Parses an 'Double' from the input 'FlagArgument'
--
-- Arguments:
--
--    *@argument@: the input argument
--
-- Returns:
--
--    * 'Nothing' if the argument cannot be parsed to the type or the @parsed
--    value ('Just')@ if it can.
doubleParser :: FlagArgument -> Maybe Double
doubleParser = valueParser

-- | Parses an 'Float' from the input 'FlagArgument'
--
-- Arguments:
--
--    *@argument@: the input argument
--
-- Returns:
--
--    * 'Nothing' if the argument cannot be parsed to the type or the @parsed
--    value ('Just')@ if it can.
floatParser :: FlagArgument -> Maybe Float
floatParser = valueParser

-- | Parses an 'Array' from the input 'FlagArgument'
--
-- Arguments:
--
--    *@argument@: the input argument
--
-- Returns:
--
--    * 'Nothing' if the argument cannot be parsed to the type or the @parsed
--    value ('Just')@ if it can.
arrayParser :: Read a => FlagArgument -> Maybe [a]
arrayParser = valueParser

-- | Parses a 'Char' from the input 'FlagArgument'
--
-- Arguments:
--
--    *@argument@: the input argument
--
-- Returns:
--
--    * 'Nothing' if the argument cannot be parsed to the type or the @parsed
--    value ('Just')@ if it can.
charParser :: FlagArgument -> Maybe Char
charParser arg = case arg of
                     (FlagMissing _)      -> Nothing
                     (FlagValueMissing _) -> Nothing
                     (FlagValue _ value)  -> if length value /= 1
                                             then Nothing
                                             else Just $ head value

-- | Parses a 'String' from the input 'FlagArgument'
--
-- Arguments:
--
--    *@argument@: the input argument
--
-- Returns:
--
--    * 'Nothing' if the argument cannot be parsed to the type or the @parsed
--    value ('Just')@ if it can.
stringParser :: FlagArgument -> Maybe String
stringParser arg = case arg of
                       (FlagMissing _)      -> Nothing
                       (FlagValueMissing _) -> Nothing
                       (FlagValue _ value)  -> Just value

-- | Parses a 'Bool' from the input 'FlagArgument'
--
-- Arguments:
--
--    *@argument@: the input argument
--
-- Returns:
--
--    * @'False' ('Just')@ if the flag is missing.
--
--    * @'True' ('Just')@ if the flag value is missing.
--
--    * 'Nothing' if the argument cannot be parsed to the type or the @parsed
--    value ('Just')@ if it can.
boolParser :: FlagArgument -> Maybe Bool
boolParser arg = case arg of
                     (FlagMissing _)      -> Just False
                     (FlagValueMissing _) -> Just True
                     (FlagValue _ value)  -> readMaybe value

-- | A predefined set of flag configurations for a boolean flag.
--
-- Defines a set of configurations that specifies:
--
--    * The parser to be 'boolParser'.
--
--    * The default value to be 'False'.
--
--    * The empty value to be 'True'
--
-- This defines a default boolean flag behavior such that if the flag is
-- missing then it is treated as 'False', if the flag is present (i.e
-- @--help@) then it is 'True'.
boolFlag :: [FlagConf Bool]
boolFlag = [ parser boolParser
           , defaultIs False
           , emptyValueIs True
           ]

{-# ANN module "HLint: ignore Use camelCase" #-}
