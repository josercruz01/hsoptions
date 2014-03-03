module System.Console.HsOptions(
    make,
    get,
    intFlag,
    stringFlag,
    flagToData,
    combine,
    process,
    Flag(..),
    FlagData,
    FlagError(..),
    FlagResults,
    ProcessResults,
    ArgsResults
) where

import Data.Maybe
import Text.Read(readMaybe)
import qualified Data.Map as Map

data Flag a = Flag String String (String -> Maybe a)
data FlagError = FlagError String deriving (Show)
type FlagData = Map.Map String String

type FlagResults = Map.Map String String
type ArgsResults = [String]
type ProcessResults  = (FlagResults, ArgsResults)

get ::  FlagResults -> Flag a ->  a
get result (Flag name _ parser) = fromJust $ parser argValue
    where argValue :: String
          argValue = fromJust $ Map.lookup name result

combine :: [FlagData] -> FlagData
combine = foldl Map.union Map.empty

flagToData :: Flag a -> FlagData
flagToData (Flag name help _) = Map.fromList [(name, help)]

process :: FlagData -> [String] -> Either [FlagError] ProcessResults
process = undefined

make :: (String, String, String -> Maybe a) -> Flag a
make (name, help, parser) = Flag name help parser

{- Flag parsers -}
intFlag :: String -> Maybe Int
intFlag = readMaybe

stringFlag :: String -> Maybe String
stringFlag = Just
