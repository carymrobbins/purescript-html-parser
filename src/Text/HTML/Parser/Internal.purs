module Text.HTML.Parser.Internal where

import Prelude
import Control.Alt
import Control.Apply
import Data.Either
import Data.List (List(..))
import Data.Maybe
import Data.Tuple

import Text.Parsing.StringParser
import Text.Parsing.StringParser.Combinators
import Text.Parsing.StringParser.String (eof, string)

import Text.HTML.Parser.Combinators
import Text.HTML.Parser.Types

parseHTML :: String -> Either ParseError (List HTML)
parseHTML s = flip runParser s $ many parseNode <* eof

parseElement = do
  Tuple name attrs <- parseOpenTag
  children <- many parseNode
  parseCloseTag name
  pure $ Element name attrs children

parseNode = fix \_ ->
  try parseElement <|>
  try parseVoidElement <|>
  parseTextNode

parseOpenTag :: Parser (Tuple String (List Attribute))
parseOpenTag = do
  notClosedTag
  char '<'
  name <- parseTagName
  skipSpaces
  attrs <- parseAttributes
  skipSpaces
  char '>'
  pure $ Tuple name attrs

parseCloseTag :: String -> Parser Char
parseCloseTag name = do
  string "</"
  string name
  char '>'

notClosedTag :: Parser Unit
notClosedTag = notFollowedBy $ string "</"

parseVoidElement = do
  notClosedTag
  char '<'
  name <- parseTagName
  skipSpaces
  attrs <- parseAttributes
  skipSpaces
  string "/>"
  pure $ VoidElement name attrs

parseTagName :: Parser String
parseTagName = catChars <$> many1 (satisfy isAlphaNumeric)

parseTextNode :: Parser HTML
parseTextNode = TextNode <<< catChars <$> many1 (noneOf ['<', '>'])

parseAttributes :: Parser (List Attribute)
parseAttributes = sepEndBy parseAttribute skipSpaces

parseAttribute :: Parser Attribute
parseAttribute = do
  name <- parseAttributeName
  skipSpaces
  maybeEquals <- optionMaybe $ char '='
  value <- case maybeEquals of
    Nothing -> pure ""
    Just _ -> parseAttributeValue
  pure $ Attribute name value

parseAttributeName :: Parser String
parseAttributeName = catChars <$> many1 (noneOf [' ', '"', '\'', '>', '/', '='])

parseAttributeValue :: Parser String
parseAttributeValue = do
  maybeOpenChar <- optionMaybe $ skipSpaces *> (char '"' <|> char '\'')
  case maybeOpenChar of
    Nothing -> catChars <$> many1 (noneOf [' ', '\t', '\n', '\r', '>', '/'])
    Just openChar -> do
      value <- catChars <$> many (noneOf [openChar])
      char openChar
      pure value
