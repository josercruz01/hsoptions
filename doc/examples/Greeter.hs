module Greeter where

import System.Console.HsOptions

userNameFlag :: Flag (Maybe String)
userNameFlag = make ("user_name", "the user name of the app", [maybeParser stringParser, 
                                                               isOptional])

flagData :: FlagData
flagData = combine [flagToData userNameFlag]

sayHello :: FlagResults -> IO ()
sayHello flagResults = case (get flagResults userNameFlag) of
    Nothing -> return ()
    Just name -> putStrLn $ "User name: " ++ name
