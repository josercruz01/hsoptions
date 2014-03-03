import System.Console.HsOptions
import System.Environment

{- Begin Flag definitions -}
userIdFlag :: Flag Int
userIdFlag = make ("user_id", "the user id of the app", intFlag)

flagData :: FlagData
flagData = flagToData userIdFlag
{- End Flag definitions -}

{- Function to be executed if there was any errors parsing the flags -}
main_errors :: [FlagError] -> IO ()
main_errors errors = do
    let errorMessages = [er | FlagError er <- errors]
    putStrLn "Error while running the program:"
    mapM_ putStrLn errorMessages
    putStrLn ""

{- Function to be executed if there was no errors parsing the flags -}
main_success :: ProcessResults -> IO ()
main_success (flagResults, _argsResults) = do
    let userId = get flagResults userIdFlag
    putStrLn $ "Main.hs: User id: " ++ show (userId)
    putStrLn ""

main :: IO ()
main = do
    args <- getArgs -- get the arguments
    let result = process flagData args -- process the arguments
    either main_errors main_success result -- handle the results
