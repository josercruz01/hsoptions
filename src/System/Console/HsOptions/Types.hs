{- |
Module      :  System.Console.HsOptions.Types
Description :  Data types and Types for HsOptions
Copyright   :  (c) Jose Raymundo Cruz (jose.r.cruz01@gmail.com)
License     :  Apache-2.0

Maintainer  :  Jose Raymundo Cruz (jose.r.cruz01@gmail.com)
Stability   :  stable
Portability :  portable

Modules that contains all types and data types created in the library.
-}
module System.Console.HsOptions.Types
where

import qualified Data.Map as Map

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
data FlagError = FlagNonFatalError String
               | FlagFatalError String

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
-- It is used to create global validation after the flags are processed. If
-- the result is a 'Nothing' then the rule passed. Otherwise if a
-- @'Just' err'@ is returned then the ruled failed with the message \"@err@\".
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

data OperationToken = OperationTokenAssign
                    | OperationTokenAppend
                    | OperationTokenAppend'
                    | OperationTokenPrepend
                    | OperationTokenPrepend'
                    deriving (Eq)

data FlagValueToken = FlagValueTokenEmpty
                    | FlagValueToken String

data Token = FlagToken String OperationToken FlagValueToken
           | ArgToken String

type DefaultOp = Map.Map String OperationToken

-- | Making 'FlagError' an instance of 'Show'
instance Show FlagError where
  -- | To show a 'FlagFatalError' we just return the error message
  show (FlagFatalError err) = err

  -- | To show a 'FlagNonFatalError' we just return the error message
  show (FlagNonFatalError err) = err

{-# ANN module "HLint: ignore Use camelCase" #-}
