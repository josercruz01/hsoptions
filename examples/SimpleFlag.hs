import System.Console.HsOptions

userName = make ( "database"
                , "the user name of the app"
                , [ parser stringParser
                  , defaultIs "prod.local"
                  , emptyValueIs "prod.local"
                  ]
                )
userAge = make ("age", "the age of the user", [parser intParser])

flagData = combine [flagToData userName]

main :: IO ()
main = processMain "Simple example for HsOptions."
                   flagData
                   success
                   failure
                   defaultDisplayHelp

success :: ProcessResults -> IO ()
success (flags, args) = putStrLn ("Hello " ++ flags `get` userName)

failure :: [FlagError] -> IO ()
failure errs = do putStrLn "Some errors occurred:"
                  mapM_ print errs

