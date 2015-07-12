module Text.HTML.Parser where

import Prelude
import Control.Alt
import Control.Apply
import Data.Char
import Data.Either
import Data.Foldable
import Data.List
import Data.Tuple

import Text.Parsing.Parser
import Text.Parsing.Parser.Combinators
import Text.Parsing.Parser.Pos
import Text.Parsing.Parser.String

data HTML
  = Element String (List Attribute) (List HTML)
  | VoidElement String (List Attribute)
  | TextNode String
  | CommentNode String

instance eqHTML :: Eq HTML where
  eq (Element name1 attrs1 children1) (Element name2 attrs2 children2) =
    name1 == name2 && attrs1 == attrs2 && children1 == children2
  eq (VoidElement name1 attrs1) (VoidElement name2 attrs2) =
    name1 == name2 && attrs1 == attrs2
  eq (TextNode text1) (TextNode text2) = text1 == text2
  eq (CommentNode text1) (CommentNode text2) = text1 == text2

instance showHTML :: Show HTML where
  show (Element name attrs children) =
    "Element " ++ show name ++ " (" ++ show attrs ++ ") (" ++ show children ++ ")"
  show (VoidElement name attrs) =
    "VoidElement " ++ show name ++ " (" ++ show attrs ++ ")"
  show (TextNode text) = "TextNode " ++ show text
  show (CommentNode text) = "CommentNode " ++ show text

data Attribute = Attribute String String

instance eqAttribute :: Eq Attribute where
  eq (Attribute k1 v1) (Attribute k2 v2) = k1 == k2 && v1 == v2

instance showAttribute :: Show Attribute where
  show (Attribute k v) = "Attribute " ++ show k ++ " " ++ show v

parseHTML :: String -> Either ParseError HTML
parseHTML s = runParser s $ (try parseElement <|> parseVoidElement) <* eof

parseElement = do
  name <- parseOpenTag
  children <- many parseElement
  parseCloseTag name
  return $ Element name Nil children

parseOpenTag :: Parser String String
parseOpenTag = do
  notFollowedBy $ string "</"
  char '<'
  name <- parseTagName
  char '>'
  return name

parseCloseTag :: String -> Parser String Char
parseCloseTag name = do
  string "</"
  string name
  char '>'

parseVoidElement = do
  char '<'
  name <- parseTagName
  string "/>"
  return $ VoidElement name Nil

parseTagName :: Parser String String
parseTagName = fold <<< map toString <$> some (satisfy isAlphaNumeric)

isAlphaNumeric :: Char -> Boolean
isAlphaNumeric c =
  code >= toCharCode '0' && code <= toCharCode '9' ||
  code >= toCharCode 'A' && code <= toCharCode 'Z' ||
  code >= toCharCode 'a' && code <= toCharCode 'z'
  where
  code = toCharCode c

prettyError (ParseError e@{ position = Position pos }) =
  e.message ++ " (" ++ show pos.line ++ "," ++ show pos.column ++ ")"
