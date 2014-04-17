HsOptions
========

HsOptions is a Haskell library that supports command-line flag processing.

It is equivalent to `getOpt()`, but for `Haskell`, and with a lot of neat 
extra features. Tipically, an application specifies what flags is expecting 
from the user -- like `--user_id` or `-file <filepath>` -- somehow in the 
code, `HsOptions` provides a declarative way to define the flags in the 
code by using the `make` method.

Most flag processing libraries requires all the flags to be defined in a 
single point, such as the main file, but `HsOptions` allows the flags to be 
scattered around the code, promoting code reuse and scalability. A module 
defines the flags it needs and when this module is used in other modules 
it's flags are handled by `HsOptions`.

Another important feature of `HsOptions` is that it can process flags 
from text files as well as from command-line. This feature is available 
with the use of the special `--usingFile <filename>` flag. 

For example:


    # inside 'file1.conf'
    --user_name batman
    --pretty

... when running the `Program.hs` haskell program:

    $ runhaskell Program.hs --debug --usingFile file1.conf -f

    === is evaluates the same as ==== >

    $ runhaskell Program.hs --debug --user_name batman --pretty -f


Each configuration file is expanded after it is processed, so it can 
include more configuration files and create a tree. This is useful 
to create different environments, like production.conf, dev.conf and 
qa.conf just to name a few.

[![Build Status](https://travis-ci.org/josercruz01/hsoptions.svg?branch=master)](https://travis-ci.org/josercruz01/hsoptions)

Table of contents
=================

- [HsOptions](#hsoptions)
- [Table of contents](#table-of-contents)
- [Install](#install)
- [Examples](#examples)
- [API](#api)
    - [Creating streams](#creating-streams)
    - [Bacon.fromBinder for custom streams](#baconfrombinder-for-custom-streams)
- [Build](#build)

Install
=======

The library depends on cabal 
([Install Cabal](http://www.haskell.org/cabal/download.html)).

To install using cabal:

    cabal install hsoptions
    
Examples
========

See [Examples](https://github.com/josercruz01/hsoptions/tree/master/examples) 
for more examples.

This program defines two flags (`user_name` of type `String` and `age` of type 
`Int`) and in the `main` function prints the name and the age plus 5:  

```haskell
-- Program.hs
import System.Console.HsOptions

userName = make ( "user_name"
                , "the user name of the app"
                , [ parser stringParser
                  , aliasIs ["u"]
                  ]
                )
userAge = make ("age", "the age of the user", [parser intParser])

flagData = combine [flagToData userName, flagToData userAge]

main :: IO ()
main = processMain "Simple example for HsOptions."
                   flagData
                   success
                   failure
                   defaultDisplayHelp

success :: ProcessResults -> IO ()
success (flags, args) = do let nextAge = (flags `get` userAge) + 5
                           putStrLn ("Hello " ++ flags `get` userName)
                           putStrLn ("In 5 years you will be " ++
                                     show nextAge ++
                                     " years old!")

failure :: [FlagError] -> IO ()
failure errs = do putStrLn "Some errors occurred:"
                  mapM_ print errs
```

You can run this program in several ways:
```
    $ runhaskell Program.hs --user_name batman --age 23
    Hello batman
    In 5 years you will be 28 years old!
```
... or:
```
    $ runhaskell Program.hs --user_name batman --age ten
    Some errors occurred:
    Error with flag '--age': Value 'ten' is not valid
```
... or:
```
    $ runhaskell Program.hs --help
    Simple example for HsOptions.
        --age        the age of the user
    -u  --user_name  the user name of the app
        --usingFile  read flags from configuration file
    -h  --help       show this help
```
API
===

Defining flags
----------------
A flag is defined using the `make` method. It takes the name of the flag,
the help text and the parser. The parser specified how to parse the string 
value of the flag to the correct type. A set of default parsers are 
provided in the library for common types.

To define a flag of type `Int`:

```haskell
    age :: Flag Int
    age = make ("age", "age of the user", [parser intParser])
```

To define the same flag of type `Maybe Int`:

```haskell
    age :: Flag (Maybe Int)
    age = make ("age", "age of the user", [maybeParser intParser])
```

The function `maybeParser` is a wrapper for a parser of any type that 
converts that parser to a `Maybe` data type, allowing the value to be 
`Nothing`. This is used mostly for optional flags.

Instead of `intParser` the user can specify his custom function to parse 
the string value to the corresponding flag type. This is useful to allow
the user to create flags of any custom type.

Process flags
-----------------------------------

To process the flags the `processMain` method is used. This method serves 
as a middle man between the real `main` and the flag processing. 
Takes 5 arguments:

* The description of the program: used when printing the help text.
* A collection of all the defined flags
* Three callback functions:
    * Success callback: called with the process results if no errors occurred
    * Failure callback: called if any error while processing flags occurred
    * Display help callback: called if the user sent the `--help` flag

This is an example on how to call the `processMain` method:

```haskell
import System.Console.HsOptions

-- flags definitions
name = make ("name", "the name of the user", [parser stringParser])
age = make ("age", "the age of the user", [parser intParser])

-- collection of all flags
all_flags = combine [flagToData age, flagToData name]

-- real main
main = processMain "Example program for processMain"
                   all_flags
                   successMain
                   defaultDisplayErrors
                   defaultDisplayHelp
                   
-- new main function
successMain (flags, args) = putStrLn $ flags `get` name                 
```

In this example the provided implementations for the failure and the display 
help callback were used (`defaultDisplayErrors` and `defaultDisplayHelp`),
so that we do not need to define how to print errors or how to print help.

As mentioned before, if no errors were found then `successMain` function is 
called. The argument sent is a tuple (`FlagResults`, `ArgsResults`).
`FlagResults` is a data structure that can be used to get the flag's 
value with the `get` method. `ArgResults` is just a list of the non-flag 
positional arguments.


Build
=====

Build from source using `build` (build and run tests):

    $ ./build
    
Or using cabal:

    $ cabal build     -- builds the text
    $ cabal test      -- runs all tests

