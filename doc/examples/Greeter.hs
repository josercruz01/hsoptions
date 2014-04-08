module Greeter where

import System.Console.HsOptions

userNameFlag :: Flag (Maybe String)
userNameFlag = make ("user_name", "the user name of the app", [maybeParser stringParser, 
                                                               isOptional,
                                                               operation append])

userLastName :: Flag String
userLastName = make ("user_last_name",
                     "the last name of the app. default=wayne if username=bruce",
                     [parser stringParser, 
                      defaultIf "wayne" (\ fr-> get fr userNameFlag == Just "bruce"),
                      emptyValueIs ""])


flagData :: FlagData
flagData = combine [flagToData userNameFlag, flagToData userLastName]

sayHello :: FlagResults -> IO ()
sayHello flagResults = do case (get flagResults userNameFlag) of
                            Nothing -> return ()
                            Just name -> putStrLn $ "Greeter.hs: User name: " ++ name
                          putStrLn $ "Greeter.hs: User last name: " ++ get flagResults userLastName

