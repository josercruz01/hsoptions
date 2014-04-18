import System.Console.HsOptions

userName = make ( "database"
    , "the user name of the app"
    , [ parser stringParser
    , defaultIs "prod.local"
    , emptyValueIs "prod.local"
    ]
    )
userAge = make ("age", "the age of the user", [parser intParser])

log_memory = make ( "log_memory"
    , "if set to true the memory usage will be logged"
    , boolFlag)


log_output = make ( "log_output"
    , "where to save the log. required if 'log_memory' is true"
    , [ maybeParser stringParser
    , requiredIf (\ flags -> flags `get` log_memory == True)
    ]
    )

flagData = combine [flagToData userName, flagToData log_output, flagToData log_memory]

main :: IO ()
main = processMain "Simple example for HsOptions."
                   flagData
                   success
                   failure
                   defaultDisplayHelp

success :: ProcessResults -> IO ()
success (flags, args) = do putStrLn ("Hello " ++ flags `get` userName)
                           putStrLn ("log_memory " ++ show (flags `get` log_memory))
                           putStrLn ("log_output " ++ show (flags `get` log_output))

failure :: [FlagError] -> IO ()
failure errs = do putStrLn "Some errors occurred:"
                  mapM_ print errs

