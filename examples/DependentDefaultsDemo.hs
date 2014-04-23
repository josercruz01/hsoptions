import System.Console.HsOptions

userName = make ("user_name", "the user", [parser stringParser])

movie = make ( "movie"
             , "the movie of the user"
             , [ parser stringParser
               , defaultIf (\ flags ->
                     if flags `get` userName == "neo"
                     then Just "matrix"
                     else if flags `get` userName == "bruce"
                          then Just "batman"
                          else Nothing)
               ]
             )

flagData = combine [flagToData userName, flagToData movie]

main :: IO ()
main = processMain "Simple example for HsOptions."
                   flagData
                   success
                   defaultDisplayErrors
                   defaultDisplayHelp

putFlag :: Show a => (String, FlagResults, Flag a) -> IO ()
putFlag (name, flags, flag) =
    putStrLn $ name ++ ": " ++ show (flags `get` flag)

success :: ProcessResults -> IO ()
success (flags, args) = do putFlag ("user_name", flags, userName)
                           putFlag ("movie", flags, movie)
