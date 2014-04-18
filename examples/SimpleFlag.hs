import System.Console.HsOptions

flagB = make ( "flagB"
                , "the user name of the app"
                , [ parser intParser
                  ]
                )
flagA = make ( "flagA"
                , "the user name of the app"
                , [ parser intParser
                  , aliasIs ["u"]
                  ]
                )
database = make ("database", "the age of the user", [parser stringParser])

flagData = combine [flagToData flagB,flagToData flagA, flagToData database]

main :: IO ()
main = processMain "Simple example for HsOptions."
                   flagData
                   success
                   defaultDisplayErrors
                   defaultDisplayHelp

success :: ProcessResults -> IO ()
success (flags, args) = do putStrLn $ "database: " ++ flags `get` database
                           putStrLn $ "flagA: " ++ show (flags `get` flagA)
                           putStrLn $ "flagB: " ++ show (flags `get` flagB)
                           putStrLn $ "args: " ++ show args
