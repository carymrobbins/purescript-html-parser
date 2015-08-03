module Text.HTML.Parser.Combinators where

import Prelude
import Control.Alt
import Control.Apply
import Data.Char
import Data.Foldable
import Data.List (List(..))
import qualified Data.String.Unsafe as Unsafe

import Text.Parsing.StringParser
import Text.Parsing.StringParser.Combinators
import Text.Parsing.StringParser.String

-- | Fail if the specified parser matches.
notFollowedBy :: forall a. Parser a -> Parser Unit
notFollowedBy p = try $ (try p *> fail "Negated parser succeeded") <|> return unit

-- | Match a character satisfying the specified predicate.
satisfy :: (Char -> Boolean) -> Parser Char
satisfy f = try do
  c <- anyChar
  if f c then return c
         else fail "Character did not satisfy predicate"

-- | Match the specified character
char :: Char -> Parser Char
char = map Unsafe.char <<< string <<< toString

-- | Match a whitespace character.
whiteSpace :: Parser String
whiteSpace = do
  cs <- many $ satisfy \c -> c == '\n' || c == '\r' || c == ' ' || c == '\t'
  return $ catChars cs

catChars :: List Char -> String
catChars = fold <<< map toString

isAlphaNumeric :: Char -> Boolean
isAlphaNumeric c =
  code >= toCharCode '0' && code <= toCharCode '9' ||
  code >= toCharCode 'A' && code <= toCharCode 'Z' ||
  code >= toCharCode 'a' && code <= toCharCode 'z'
  where
  code = toCharCode c

-- | Skip whitespace characters.
skipSpaces :: Parser Unit
skipSpaces = do
  whiteSpace
  return unit

-- | Match any character not in the array.
noneOf :: Array Char -> Parser Char
noneOf ss = satisfy (flip notElem ss)
