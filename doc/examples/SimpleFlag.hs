import System.Console.HsOptions
import System.Environment
import qualified Greeter as Greeter

{- Begin Flag definitions -}
userIdFlag :: Flag Int
userIdFlag = make ("user_id", "the user id of the app", [parser intParser])

helpFlag :: Flag Bool
helpFlag = make ("help", "show this help", boolFlag)

description :: String
description = "Simple Haskell program\n" ++
              "Just prints a simple message based on the input flags"

flagData :: FlagData
flagData = combine [
            combine [ flagToData userIdFlag,
                      flagToData helpFlag],
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
main_success (flagResults, _argsResults) = if get flagResults helpFlag
  then showHelp description flagData 
  else do let userId = get flagResults userIdFlag
          putStrLn $ "Main.hs: User id: " ++ show userId
          Greeter.sayHello flagResults
          putStrLn ""

main :: IO ()
main = do
    args <- getArgs -- get the arguments
    let result = process flagData args -- process the arguments
    either main_errors main_success result -- handle the results
