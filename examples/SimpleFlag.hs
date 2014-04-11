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

