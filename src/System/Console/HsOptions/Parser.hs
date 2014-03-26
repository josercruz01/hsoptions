module System.Console.HsOptions.Parser (
   parseInput, 
   Token(..),
   OperationToken(..),
   FlagValueToken(..)
)where

import Text.ParserCombinators.Parsec
import Data.Char
import Control.Monad(void)

data OperationToken = OperationTokenAssign
                    | OperationTokenAppend
                    deriving (Eq)

data FlagValueToken = FlagValueTokenEmpty
                    | FlagValueToken String 

data Token = FlagToken String OperationToken FlagValueToken
           | ArgToken String

instance Show FlagValueToken where
   show FlagValueTokenEmpty = "''"
   show (FlagValueToken val) = "'" ++ val ++ "'"

instance Show Token where
   show (FlagToken name op val) = "Flag( " ++ name ++ show op ++ show val ++ " )"
   show (ArgToken val) = "Arg( " ++ val ++ " )"

instance Show OperationToken where
   show t = head [k | (k, v) <- operationsKeyMap, v == t]

operationsKeyMap :: [(String, OperationToken)]
operationsKeyMap = [
    ("+=", OperationTokenAppend),
    ("=", OperationTokenAssign)
  ]

operationKeywords :: [String]
operationKeywords = [k | (k,_) <- operationsKeyMap]

operationTokenFor :: String -> OperationToken
operationTokenFor s = head [v | (k, v) <- operationsKeyMap, k == s]

flag :: GenParser Char st Token
flag = do name <- flagName
          op <- flagOperation
          value <- flagValue
          return (FlagToken name op value)


flagName :: GenParser Char st String
flagName = do spaces
              flagPrefix
              l1 <- letter
              ls <- validFlagChars 
              return (l1:ls)

flagPrefix :: GenParser Char st ()
flagPrefix = void $ try (string "--") <|> string "-"

flagOperation :: GenParser Char st OperationToken
flagOperation = try operation <|> do void space <|> eof
                                     return OperationTokenAssign

flagValue :: GenParser Char st FlagValueToken
flagValue = (try (flagName >> fail "") >> return FlagValueTokenEmpty) <|> 
            (do arg <- argument; return (FlagValueToken arg)) <|>
            return FlagValueTokenEmpty

argument :: GenParser Char st String
argument = spaces >> many1 (satisfy (not . isSpace))

cmdLineArg :: GenParser Char st Token
cmdLineArg = do arg <- argument
                return (ArgToken arg)

operation :: GenParser Char st OperationToken
operation = do op <- choice (map (\s -> try (spaces >> string s)) operationKeywords)
               return (operationTokenFor op)

validFlagChars :: GenParser Char st String
validFlagChars = many (oneOf "-_" <|> alphaNum)

manyToken :: GenParser Char st [Token]
manyToken = many (try flag <|> 
                  try cmdLineArg)

parseInput' :: String -> Either ParseError [Token]
parseInput' = parse manyToken "Top level parse error" 

parseInput :: String -> [Token]
parseInput input = case parseInput' input of
                      Left err -> error (show err)
                      Right result -> result

