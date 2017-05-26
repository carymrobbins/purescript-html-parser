module Text.HTML.Parser.Combinators where

import Prelude
import Control.Alt ((<|>))
import Data.Char (toCharCode)
import Data.String (singleton, joinWith)
import Data.Foldable (fold, notElem)
import Data.List (List)
import Data.String.Unsafe as Unsafe

import Text.Parsing.StringParser (Parser, fail, try)
import Text.Parsing.StringParser.Combinators (many)
import Text.Parsing.StringParser.String (anyChar, string)

-- | Fail if the specified parser matches.
notFollowedBy :: forall a. Parser a -> Parser Unit
notFollowedBy p = try $ (try p *> fail "Negated parser succeeded") <|> pure unit

-- | Match a character satisfying the specified predicate.
satisfy :: (Char -> Boolean) -> Parser Char
satisfy f = try do
  c <- anyChar
  if f c then pure c
         else fail $ joinWith "" [ "Character ", (show c),  " did not satisfy predicate" ]

-- | Match the specified character
char :: Char -> Parser Char
char = map Unsafe.char <<< string <<< singleton

-- | Match a whitespace character.
whiteSpace :: Parser String
whiteSpace = do
  cs <- many $ satisfy \c -> c == '\n' || c == '\r' || c == ' ' || c == '\t'
  pure $ catChars cs

catChars :: List Char -> String
catChars = fold <<< map singleton

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
  _ <- whiteSpace
  pure unit

-- | Match any character not in the array.
noneOf :: Array Char -> Parser Char
noneOf ss = satisfy (flip notElem ss)
