{- |
Module      :  System.Console.HsOptions.ParserCore
Description :  Core features of the Parser module
Copyright   :  (c) Jose Raymundo Cruz (jose.r.cruz01@gmail.com)
License     :  Apache-2.0

Maintainer  :  Jose Raymundo Cruz (jose.r.cruz01@gmail.com)
Stability   :  stable
Portability :  portable

Core functions of the 'System.Console.HsOptions.Parser.Parser'.
-}

module System.Console.HsOptions.ParserCore
where

import Control.Monad(void)
import Data.Char
import Data.Maybe
import Text.ParserCombinators.Parsec
import qualified Data.Map as Map
import System.Console.HsOptions.Types

-- | Map of operation keywords to the corresponding operation token.
operationsKeyMap :: [(String, OperationToken)]
operationsKeyMap = [ ("+=!", OperationTokenAppend')
                   , ("+=", OperationTokenAppend)
                   , ("=+!", OperationTokenPrepend')
                   , ("=+", OperationTokenPrepend)
                   , ("=", OperationTokenAssign)
                   ]

-- | Returns a list of all operation keywords.
operationKeywords :: [String]
operationKeywords = [k | (k,_) <- operationsKeyMap]

-- | Returns the corresponding operation token for the input keyword.
operationTokenFor :: String -> OperationToken
operationTokenFor s = head [v | (k, v) <- operationsKeyMap, k == s]

-- | Parses a flag.
--
-- A flag consist of a 'name', followed by an 'flagOperation' and
-- a 'value'.
--
-- Returns:
--
--    * A 'FlagToken'.
flag :: DefaultOp -> GenParser Char st Token
flag defaultOp = do name <- flagName
                    op <- flagOperation name defaultOp
                    value <- flagValue
                    return (FlagToken name op value)


-- | Parses the name of a flag.
--
-- The name must follow the pattern of: \"'flagPrefix' 'letter'
-- 'validFlagChars'\".
--
-- Returns:
--
--    * A string with the flag name (without the prefix part).
flagName :: GenParser Char st String
flagName = do spaces
              flagPrefix
              l1 <- letter
              ls <- validFlagChars
              return (l1:ls)

-- | Parses the flag prefix.
--
-- A flag prefix is a double dash (--) or a single dash (-).
flagPrefix :: GenParser Char st ()
flagPrefix = void $ try (string "--") <|> string "-"

-- | Parses a flag operation.
--
-- Flag operations will be parsed from the keywords defined in the
-- 'operationsKeyMap'.
flagOperation :: String -> DefaultOp -> GenParser Char st OperationToken
flagOperation name defaultOp = try operation <|>
                               do spaceOrEof
                                  return defaultOp'
  where defaultOp' = fromMaybe OperationTokenAssign (Map.lookup name defaultOp)

-- | Parses a space or the end of file character.
spaceOrEof :: GenParser Char st ()
spaceOrEof = void space <|> eof

-- | Parses a word that is not a flag.
--
-- \"Not a flag\" parses anything that is not parsed by 'flag' parser.
notFlag :: GenParser Char st String
notFlag = do spaces
             choice [ try (quotedString '"')
                    , try twoDash
                    , try singleDash
                    , try nf1
                    , try nf2
                    , nf3
                    ]
  where nf1 = do c1 <- string "--"
                 c2 <- satisfy (not . isLetter)
                 rest <- allButSpace
                 return (c1 ++ [c2] ++ rest)

        nf2 = do c1 <- string "-"
                 c2 <- satisfy (\s -> (not . isLetter) s && s /= '-')
                 rest <- allButSpace
                 return (c1 ++ [c2] ++ rest)

        nf3 = do c1 <- noneOf "-"
                 rest <- allButSpace
                 return (c1:rest)

        twoDash = do c1 <- string "--"
                     spaceOrEof
                     return c1

        singleDash = do c1 <- string "-"
                        spaceOrEof
                        return c1

-- | Parses a quoted string using the @character@ for quotes.
--
-- Arguments:
--
--    *@character@: the character used as quotes.
quotedString :: Char -> GenParser Char st String
quotedString c = do _ <- char c
                    middle <- many (noneOf [c])
                    void (char c) <|> eof
                    return middle

-- | Parses a flag value.
--
-- A flag value is parsed with 'notFlag'. If this parser fails then
-- 'FlagValueTokenEmpty' is returned.
flagValue :: GenParser Char st FlagValueToken
flagValue = try getValue <|> return FlagValueTokenEmpty
  where getValue = do value <- notFlag
                      return (FlagValueToken value)

-- | Parses all characters until a space is found.
allButSpace :: GenParser Char st String
allButSpace = many (satisfy (not . isSpace))

-- | Parses a command line argument.
--
-- A command argument is an argument that is not a flag ('notFlag').
cmdLineArg :: GenParser Char st Token
cmdLineArg = do arg <- notFlag
                return (ArgToken arg)

-- | Parses a flag operation.
--
-- Returns: a token representing for that operation.
operation :: GenParser Char st OperationToken
operation = do op <- choice $ map aux operationKeywords
               return $ operationTokenFor op
  where aux op = try (spaces >> string op)

-- | Parses and returns the characters that are valid for a flag.
validFlagChars :: GenParser Char st String
validFlagChars = many (oneOf "-_" <|> alphaNum)

-- | Parses many flags and/or many positional arguments.
--
-- Returns:
--
--    * A list of tokens.
manyToken :: DefaultOp -> GenParser Char st [Token]
manyToken defaultOp = many (try (flag defaultOp) <|>
                      try cmdLineArg)

-- | Runs parser with the 'manyToken' parser.
parseInput' :: DefaultOp -> String -> Either ParseError [Token]
parseInput' defaultOp = parse (manyToken defaultOp ) "Top level parse error"

-- | Parses the flags from the input stream of characters to a stream of
-- tokens.
--
-- Based on the syntax of the @flags input@ this parser should not fail.
-- If there is any kind of errors while parsing an exception is thrown.
--
-- Arguments:
--
--    *@default_operations@: a map from flag name to default operation.
--
--    *@input@: the input stream of characters.
--
-- Returns:
--
--    * A stream of tokens.
--
-- Throws:
--
--    * An exception if some error with the parser occurs.
parseInput :: DefaultOp -> String -> [Token]
parseInput defaultOp input = case parseInput' defaultOp input of
                                 Left err     -> error (show err)
                                 Right result -> result

