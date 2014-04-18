{- |
Module      :  System.Console.HsOptions
Description :  Command line flag parser for Haskell
Copyright   :  (c) Jose Raymundo Cruz (jose.r.cruz01@gmail.com)
License     :  Apache-2.0

Maintainer  :  Jose Raymundo Cruz (jose.r.cruz01@gmail.com)
Stability   :  stable
Portability :  portable

HsOptions library supports command line flag parsing.

Too see an user guide and list of features go to
<https://github.com/josercruz01/hsoptions#table-of-contents>.

Flags are declared in the code by using the 'make' function, which takes the
flag's name, help text and type as arguments.

The flags are parsed from the command line stream of from a configuration file
if the @--usingFile \<filename\>@ flag is sent to the program.

A configuration file is just a text document that defines command line
arguments for our program in the standard command line syntax. This is a
simple configuration file example that defines two flags and three
positional arguments and also includes a second configuration file:

@
 # confFile1.txt
--user_id = 8
--user_name = batman
--usingFile /tmp/localConfiguration.txt
arg1
arg2
arg3
@

So doing this:

>>> runhaskell Prog.hs --usingFile confFile1.txt

Is equivalent to doing this

>>> runhaskell Prog.hs --user_id = 8 --user_name = batman ... arg2 arg3

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

    -- * Flag configurations
    parser,
    maybeParser,
    isOptional,
    emptyValueIs,
    defaultIs,
    defaultIf,
    aliasIs,
    requiredIf,

    -- * Flag parsers #parsers#
    intParser,
    floatParser,
    doubleParser,
    charParser,
    stringParser,
    boolParser,
    arrayParser,
    boolFlag,
    toMaybeParser,

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
    ArgsResults,
    FlagConf,
) where

import System.Console.HsOptions.Types
import System.Console.HsOptions.Core

{-# ANN module "HLint: ignore Use camelCase" #-}
