import System.Console.HsOptions
import qualified Greeter as Greeter

{- Begin Flag definitions -}
userIdFlag :: Flag Int
userIdFlag = make ("user_id", "the user id of the app", [parser intParser])

description :: String
description = "Simple Haskell program\n" ++
              "Just prints a simple message based on the input flags"

database :: Flag (Maybe String)
database = make ("database", "database connection string. required if user_id == -1",
    maybeParser stringParser:
    requiredIf (\ fr -> get fr userIdFlag == -1))

flagData :: FlagData
flagData = combine [
            combine [ flagToData userIdFlag,
                      flagToData database],
            Greeter.flagData
           ]

{- End Flag definitions -}

{- Function to be executed if there was any errors parsing the flags -}
main_errors :: [FlagError] -> IO ()
main_errors errors = do
    let errorMessages = [er | FlagNonFatalError er <- errors]
    let errorMessages' = [er | FlagFatalError er <- errors]
    putStrLn "Error while running the program:"
    mapM_ putStrLn (errorMessages ++ errorMessages')
    putStrLn ""

{- Function to be executed if there was no errors parsing the flags -}
main_success :: ProcessResults -> IO ()
main_success (flagResults, _argsResults) = 
   do let userId = get flagResults userIdFlag
          db = get flagResults database
      putStrLn $ "Main.hs: User id: " ++ show userId
      putStrLn $ "Main.hs: Database: " ++ show db
      Greeter.sayHello flagResults
      putStrLn ""


main :: IO ()
main = processMain description 
                   flagData
                   main_success 
                   main_errors 
                   defaultDisplayHelp
