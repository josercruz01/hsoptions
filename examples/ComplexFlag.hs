import System.Console.HsOptions
import qualified Greeter as Greeter

userIdFlag :: Flag Int
userIdFlag = make ("user_id", "the user id of the app", [parser intParser,
                                                         aliasIs ["u"]])

description :: String
description = "Complex Haskell program\n" ++
              "Just prints a simple message based on the input flags"

database :: Flag (Maybe String)
database = make ("database", "database connection string. required if user_id == -1",
    [maybeParser stringParser,
     aliasIs ["db"],
     requiredIf (\ fr -> get fr userIdFlag == -1)])

tellJoke :: Flag Bool
tellJoke = make ("tell_joke", "tells a joke", boolFlag)

profileMemory :: Flag Bool
profileMemory = make ("profile_memory", "profiles the memory of the app", boolFlag)

profileDisk :: Flag Bool
profileDisk = make ("profile_disk", "profiles the disk usage of the app", boolFlag)

flagData :: FlagData
flagData = combine [
            combine [ flagToData userIdFlag,
                      flagToData database,
                      flagToData tellJoke,
                      flagToData profileMemory,
                      flagToData profileDisk],
            Greeter.flagData,

            -- Global validation
            validate (\fr -> if get fr profileMemory && get fr profileDisk 
                             then Just "'--profile_memory' and '--profile_disk' can't be on at the same time"
                             else Nothing)
           ]

main_errors :: [FlagError] -> IO ()
main_errors errors = do
    let errorMessages = [er | FlagNonFatalError er <- errors]
    let errorMessages' = [er | FlagFatalError er <- errors]
    putStrLn "Error while running the program:"
    mapM_ putStrLn (errorMessages ++ errorMessages')
    putStrLn ""

main_success :: ProcessResults -> IO ()
main_success (flags, argsResults) = 
   do let userId = flags `get` userIdFlag -- get userId
          db = flags `get` database       -- get database

      putStrLn $ "Main.hs: Args: " ++ show argsResults
      putStrLn $ "Main.hs: User id: " ++ show userId
      putStrLn $ "Main.hs: Database: " ++ show db

      -- Call other modules
      Greeter.sayHello flags

      putStrLn $ if flags `get` tellJoke
                 then "I never make mistakes…I thought I did once; but I was wrong."
                 else "Not a time for jokes."


main :: IO ()
main = processMain description 
                   flagData
                   main_success 
                   main_errors 
                   defaultDisplayHelp
