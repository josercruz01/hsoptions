{- |
Module      :  System.Console.HsOptions.Parser
Description :  Parses the flags from the input stream for HsOptions
Copyright   :  (c) Jose Raymundo Cruz (jose.r.cruz01@gmail.com)
License     :  Apache-2.0

Maintainer  :  Jose Raymundo Cruz (jose.r.cruz01@gmail.com)
Stability   :  stable
Portability :  portable

Provides the parser features that takes a input stream of characters
and returns a stream of tokens.

These tokens are easier to handle as the syntax of the flags was enforced by
the parser.
-}
module System.Console.HsOptions.Parser (
   -- * Parser functions
   parseInput
) where

import System.Console.HsOptions.ParserCore

