HsOptions
========

HsOptions is a Haskell library that supports command-line flag processing.

It is equivalent to `getOpt()`, but for `Haskell`, and with a lot of neat extra
features. Typically, an application specifies what flags it is expecting from
the user -- like `--user_id` or `-file <filepath>` -- somehow in the code,
`HsOptions` provides a declarative way to define the flags in the code by using
the `make` function.

Most flag processing libraries requires all the flags to be defined in a single
point, such as the main file, but `HsOptions` allows the flags to be scattered
around the code, promoting code reuse and scalability. A module defines the
flags it needs and when this module is used in other modules it's flags are
handled by `HsOptions`.

`HsOptions` is completely functional, specially because no global state is
modified. The only `IO` actions performed are to get the command-line arguments
and to expand the configuration files.

Another important feature of `HsOptions` is that it can process flags from text
files as well as from command-line. This feature is available with the use of
the special `--usingFile <filename>` flag.

For example:


    # inside 'file1.conf'
    --user_name batman
    --pretty

... when running the `Program.hs` haskell program:

    $ runhaskell Program.hs --debug --usingFile file1.conf -f

    === is evaluates the same as ==== >

    $ runhaskell Program.hs --debug --user_name batman --pretty -f


Each configuration file is expanded after it is processed, so it can include
more configuration files and create a tree. This is useful to create different
environments, like production.conf, dev.conf and qa.conf just to name a few.

[![Build Status](https://travis-ci.org/josercruz01/hsoptions.svg?branch=master)](https://travis-ci.org/josercruz01/hsoptions)

Table of contents
=================

- [HsOptions](#hsoptions)
- [Table of contents](#table-of-contents)
- [Install](#install)
- [Examples](#examples)
- [API](#api)
    - [Defining flags](#defining-flags)
    - [Process flags](#process-flags)
    - [Get flag value](#get-flag-value)
    - [Optional and Required flags](#optional-and-required-flags)
    - [Configuration files](#configuration-files)
    - [Flag operations](#flag-operations)
        - [Assign](#assign)
        - [Inherit keyword](#inherit-keyword)
        - [Append](#append)
        - [Prepend](#prepend)
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
`Int`) and in the `main` function prints the name and the age plus 5. It also
adds the alias `u` to the flag `user_name`.

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

    $ runhaskell Program.hs --user_name batman --age 23
    Hello batman
    In 5 years you will be 28 years old!

... or:

    $ runhaskell Program.hs --user_name batman --age ten
    Some errors occurred:
    Error with flag '--age': Value 'ten' is not valid

... or:

    $ runhaskell Program.hs --help
    Simple example for HsOptions.
        --age        the age of the user
    -u  --user_name  the user name of the app
        --usingFile  read flags from configuration file
    -h  --help       show this help

API
===

Defining flags
----------------
A flag is defined using the `make` function. It takes the name of the flag, the
help text and the parser. The parser specified how to parse the string value of
the flag to the correct type. A set of default parsers are provided in the
library for common types.

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

The function `maybeParser` is a wrapper for a parser of any type that converts
that parser to a `Maybe` data type, allowing the value to be `Nothing`. This is
used mostly for optional flags.

Instead of `intParser` the user can specify his custom function to parse the
string value to the corresponding flag type. This is useful to allow the user to
create flags of any custom type.

Process flags
-----------------------------------

To process the flags the `processMain` function is used. This function serves as
a middle man between the real `main` and the flag processing. Takes 5 arguments:

* The description of the program: used when printing the help text.
* A collection of all the defined flags
* Three callback functions:
    * Success callback: called with the process results if no errors occurred
    * Failure callback: called if any error while processing flags occurred
    * Display help callback: called if the user sent the `--help` flag

This is an example on how to call the `processMain` function:

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

In this example, the provided implementations for the failure and the display
help callback were used (`defaultDisplayErrors` and `defaultDisplayHelp`), so
that we do not need to define how to print errors or how to print help.

As mentioned before, if no errors were found then `successMain` function is
called. The argument sent is a tuple (`FlagResults`, `ArgsResults`).
`FlagResults` is a data structure that can be used to get the flag's value with
the `get` function. `ArgResults` is just a list of the non-flag positional
arguments.

If there was any kind of errors while processing the flags the `display errors`
callback argument is called with the list of `FlagError` as argument. The user
can specify a custom function so he handles the argument as he wishes.

The third callback, `display help`, is called when the user sent the special help
flag (`--help` or `-h`). It takes the program description and all the
information of the flags as a list of (`flag_name`, `[flag_alias]`,
`flag_helptext`). The `defaultDisplayHelp` is a default implementation that
prints the helptext in a standard format, usually this is the way to go unless
the user wants to print the help text in a custom format.

Get flag value
-----------------------------------

A flag value is obtained by using the `get` function. It takes the `FlagResults`
and a defined flag as a parameter, and it will look for the value of the flag
inside the `FlagResults`. In a way you can think of `FlagResults` as a data
structure that can be queried with flags to retrieve flag values.

The `FlagResults` are obtained by processing the flags with the
[`processMain`](#process-flags) function.

The return type of `get` is the type of the flag, so if the flag is `Flag Int`
then `get` returns an `Int` (so the flag value is typed).

For a given flag:

```haskell
repeat = make ("repeat", "how many times to repeat", [parser intParser])
```

... we can grab it's value after processing like this:

```haskell
success :: (FlagResults, ArgsResults) -> IO ()
success (flags, args) = do let r = flags `get` repeat
                           putStrLn $ "The value of repeat is " ++ show r
```

Optional and Required flags
-----------------------------------
By default all flags are marked as required. If you want to make an optional
flag then two things are required:

    * First, the type of the flag must be `Flag (Maybe a)`, so that the flag can
    be `Nothing` if it was not provided and `Just value` if it was.

    * Second, the flag must be configured using the `isOptional` flag
    configuration.

Example:

```haskell
-- optional flag
database :: Flag (Maybe String)
database = make ("db", "the database", [maybeParser stringParser, isOptional])

-- required flag
app_id :: Flag Int
app_id = make ("app_id", "application to run", [parser intParser])

-- combine all flags
all_flags = combine [flagToData database, flagToData app_id]

-- main
main = processMain "Sample" all_flags success
                   defaultDisplayErrors defaultDisplayHelp

-- success main
success (flags, _) = do putStrLn $ "database: " ++ show (flags `get` database)
                        putStrLn $ "app_id: " ++ show (flags `get` app_id)
```

This is the expected behavior when getting the flag value:

    $ runprogram Program.hs
    Errors occurred while parsing flags:
    Error with flag '--app_id': Flag is required

... as you can see only `app_id` is required, but not `database`.

    $ runprogram Program.hs --app_id = 123
    database: Nothing
    app_id: 123

... value for `database` is `Nothing`.

    $ runprogram Program.hs --app_id = 123 --db = local
    database: Just "local"
    app_id: 123
    
Configuration files
=====

Flags can be processed not only from command-line input, but also from configuration text 
files. These text files are included at any point in the command-line stream by using the
special flag `--usingFile <filename>`.

When the flag processor encounters a `usingFile` it reads the content of the file and 
runs the processor again with this content, consuming the `usingFile` flag and replacing
it with all the new flags found inside the configuration file.

A configuration file can itself include other configuration files as well, by using the 
`usingFile` flag inside the file, so a tree of files can be created (a file can have a 
parent file, and a grandparent file, or a file can include multiple files to combine 
them together).

If there is any kind of error while reading the file, or there is a syntax error inside 
the file then that error is reported to the user.

This is an example of a configuration file that has comments, and that includes two more
files.


```
# combined.conf
--database = localdb
--usingFile = file1.conf
--usingFile = file2.conf
jack
jill
batman
```

```
# file1.conf
--flagA = 3
```


```
# file2.conf
--flagB = 42
```

So if we have a `Program.hs` that is configured with the flags `database`, `flagA` and `flagB`,
and that prints the remaining positional arguments, then this is the output of the program
for the following scenarios:

```
$ runprogram Program.hs --usingFile combined.conf
database: localdb
flagA: 3
flagB: 42
args: ["jack","jill","batman"]
```

We can send more arguments, or modify flags, after or before including the file:

```
$ runprogram Program.hs superman --usingFile combined.conf robin
database: localdb
flagA: 3
flagB: 42
args: ["superman", "jack","jill","batman", "robin"]
```

... as you can observe `superman` and `robin` are respectively at the start and end
of the positional arguments, that is because first `superman` is found in the input stream,
then the `usingFile combined.conf` which gets evaluated and parsed, and when this is 
complete then the processor moves to `robin` which is captured as the last positional
argument.

Here is another example on how we can override and extend the flags. We will change the
`flagA` to 1024 and will append the value `.local` to the `database` flag.

```
$ runprogram Program.hs --usingFile combined.conf --database +=! ".local" --flagA = 1024
database: localdb.local
flagA: 1024
flagB: 42
args: ["jack","jill","batman"]
```


Flag operations
=====

Flag operations allows the user to set the value of a flag based on the previous value set. 
This is useful in situations where configuration files are used, so that a child configuration
file can extend the value of a flag set in a parent configuration file.

Operations are specified when setting a value for a flag. This is the syntax to set a flag: 
`--flag_name [operation] flag_value`. If the `[operation]` is not set then the `assign (=)` 
operation is implied.


### Assign

This is the default operation. Sets the value of the flag, overwriting any previous value if 
there was any. If the user does not specifies any explicit operation then this is the 
operation used.

Example:

        $ runhaskell Program.hs --file = "/home/user/" --file = "/tmp"
        file: "/tmp"

### Inherit keyword

The `$(inherit)` keyword can be used in the flag value and will be expanded to the previous
value of the flag (or to empty string if no previous value).

Example:

        $ runhaskell Program.hs --file = "/home/user" --file = "$(inherit)/local/tmp"
        file: "/home/user/local/tmp"

... and with no previous value:

        $ runhaskell Program.hs --file = "$(inherit)/local/tmp"
        file: "/local/tmp"


### Append
It's an specification of the `$(inherit)` keyword to append the current value of the flag to 
the previous. There are two ways to append, using the `+=` symbol or the `+=!` symbol.

They are the same except that `+=` puts a space between previous value and current value (if 
there is a previous value for the flag).

They are equivalent to:

        --file += /local/tmp   <=> --file = "$(inherit) /local/tmp"   -- space in between
        --file +=! /local/tmp  <=> --file = "$(inherit)/local/tmp"    -- no space in between

Example `(+=)`:

        $ runhaskell Program.hs --warning = "1 2" --warning += "3"
        warning: "1 2 3"
        
Example `(+=!)`:

        $ runhaskell Program.hs --warning = "warn-1,2" --warning +=! ",3"
        warning: "warn-1,2,3"
 

### Prepend
It's an specification of the `$(inherit)` keyword to prepend the current value of the flag to 
the previous. There are two ways to prepend, using the `=+` symbol or the `=+!` symbol.

They are the same except that `=+` puts a space between previous value and current value (if 
there is a previous value for the flag).

They are equivalent to:

        --file =+ /local/tmp   <=> --file = "/local/tmp $(inherit)"   -- space in between
        --file =+! /local/tmp  <=> --file = "/local/tmp$(inherit)"    -- no space in between

Example `(=+)`:

        $ runhaskell Program.hs --warning = "1 2" --warning =+ "0"
        warning: "0 1 2"
        
Example `(=+!)`:

        $ runhaskell Program.hs --warning = "warn-1,warn-2" --warning =+! "warn-0,"
        warning: "warn-0,warn-1,warn-2"
 

Build
=====

Build from source using `build` (build and run tests):

    $ ./build

Or using cabal:

    $ cabal build     -- builds the text
    $ cabal test      -- runs all tests

