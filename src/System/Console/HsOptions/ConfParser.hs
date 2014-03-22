module System.Console.HsOptions.ConfParser (
  parse,
  parseFromString,

  ConfParserError(..)
) where

import Data.Maybe
import Data.Char (isSpace)
import Control.Exception

data ConfParserError = 
    ConfParserNonFatalError {-file-}String {-msg-}String 
  | ConfParserFatalError {-file-}String {-msg-}String 

data SimpleError =
    SimpleFatalError String
  | SimpleNonFatalError String

data Operation = Assign {- = -}
               -- | AppendRight {- =+ -}
               -- | AppendLeft {- += -} 
                 deriving (Show)

errorToStr :: String -> String -> String
errorToStr file message = "Error on '" ++ file ++ "': " ++ message

instance Show (ConfParserError) where
  show (ConfParserNonFatalError name message) = errorToStr name message
  show (ConfParserFatalError name message) = errorToStr name message

parse :: String -> IO (Either [ConfParserError] [String])
parse filename = do fileResult <- try $ readFile filename :: IO (Either SomeException String)
                    case fileResult of 
                        Left except -> return (Left [ConfParserFatalError filename (show except)])
                        Right content -> do let result = parseFromString filename content
                                            return result

trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace

trimLeft :: String -> String
trimLeft = dropWhile isSpace

removeEmptyLines :: [String] -> [String]
removeEmptyLines [] = []
removeEmptyLines (x:xs) = if null $ words x
  then removeEmptyLines xs 
  else x : removeEmptyLines xs

mkConfParserError :: String -> [SimpleError] -> [ConfParserError]
mkConfParserError filename = map aux 
  where aux (SimpleNonFatalError msg) = ConfParserNonFatalError filename msg
        aux (SimpleFatalError msg) = ConfParserFatalError filename msg

convertErrors :: String -> Either [SimpleError] [String] -> Either [ConfParserError] [String]
convertErrors filename (Left errs) = Left $ mkConfParserError filename errs
convertErrors _filename (Right args) = Right args

parseFromString :: String -> String -> Either [ConfParserError] [String]
parseFromString filename fileContent = convertErrors filename result
  where cleanLines = removeEmptyLines (lines fileContent)
        result = parseFromStringAux cleanLines

data Token = TokenName String 
           | TokenOp Operation
           | TokenValue String
           deriving (Show)

operations :: [(String, Operation)]
operations = [
    ("=", Assign)
  ]


type Tokenizer = String -> Maybe ([Token], String)

(+++) :: Tokenizer -> Tokenizer -> Tokenizer 
(+++) t1 t2 s =  case t1 s of
                    Nothing -> Nothing
                    Just (t1', s') -> case t2 s' of
                      Nothing -> Nothing
                      Just (t2', s'') -> Just (t1' ++ t2', s'')

isOperation :: String -> Bool
isOperation  = isJust . tokenizeOperation

startsWith :: String -> String -> Maybe String
s `startsWith` text = if take l s == text 
                    then Just (drop l s)
                    else Nothing
  where l = length text

isInvalidName :: String -> Bool
isInvalidName s = trimLeft s == "" || isOperation s

tokenizeName :: Tokenizer
tokenizeName str | isInvalidName str = Nothing
tokenizeName str = let (s:ss) = trimLeft str in aux [s] ss
  where aux acc rest 
          | isInvalidName rest = Just ([TokenName acc], rest)
          | otherwise = aux (acc ++ take 1 rest) (drop 1 rest)

tokenizeOperation :: Tokenizer
tokenizeOperation str = case listToMaybe ops of
                          Nothing -> Nothing
                          Just (op, s') -> Just ([TokenOp op], s')
  where s = trimLeft str
        ops = mapMaybe aux operations
        aux (opName, op) = case s `startsWith` opName of
                              Nothing -> Nothing
                              Just rest -> Just (op, rest)
tokenizeValue :: Tokenizer
tokenizeValue str | trimLeft str == "" = Nothing
tokenizeValue str = let s = trim str in Just ([TokenValue s], "")

tokenize :: Tokenizer
tokenize = tokenizeName +++ tokenizeOperation +++ tokenizeValue

parseSingle :: String -> Either [SimpleError] (String, Operation, String)
parseSingle line = case tokenize line of
    Nothing -> mkerror 
    Just (TokenName name:TokenOp op:TokenValue value:[], "") -> Right (name, op, value)
    _ -> mkerror 
  where mkerror = Left [SimpleNonFatalError $ "Syntax error near '" ++ line ++ "'"]

parseFromStringAux :: [String] -> Either [SimpleError] [String]
parseFromStringAux [] = Right []
parseFromStringAux (x:xs) = case parseSingle x of
        Left errs -> Left errs
        Right (name, _op, value) -> case parseFromStringAux xs of 
          Left errs -> Left errs
          Right args -> Right (name:value:args)
