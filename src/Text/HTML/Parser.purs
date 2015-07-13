module Text.HTML.Parser where

import Prelude
import Control.Alt
import Control.Apply
import Data.Char
import Data.Either
import Data.Foldable
import Data.List
import Data.Maybe
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

-- HTML parser

parseHTML :: String -> Either ParseError (List HTML)
parseHTML s = runParser s $ many parseNode <* eof
  where
  parseNode =
    try parseElement <|>
    try parseVoidElement <|>
    parseTextNode

parseElement = do
  Tuple name attrs <- parseOpenTag
  children <- many $
    try parseElement <|>
    try parseVoidElement <|>
    parseTextNode
  parseCloseTag name
  return $ Element name attrs children

parseOpenTag :: Parser String (Tuple String (List Attribute))
parseOpenTag = do
  notClosedTag
  char '<'
  name <- parseTagName
  spaces
  attrs <- parseAttributes
  spaces
  char '>'
  return $ Tuple name attrs

parseCloseTag :: String -> Parser String Char
parseCloseTag name = do
  string "</"
  string name
  char '>'

notClosedTag :: Parser String Unit
notClosedTag = notFollowedBy $ string "</"

parseVoidElement = do
  notClosedTag
  char '<'
  name <- parseTagName
  string "/>"
  return $ VoidElement name Nil

parseTagName :: Parser String String
parseTagName = catChars <$> some (satisfy isAlphaNumeric)

parseTextNode :: Parser String HTML
parseTextNode = TextNode <<< catChars <$> some (noneOf ['<', '>'])

parseAttributes :: Parser String (List Attribute)
parseAttributes = sepBy parseAttribute spaces

parseAttribute :: Parser String Attribute
parseAttribute = do
  name <- parseAttributeName
  spaces
  maybeEquals <- optionMaybe $ char '='
  value <- case maybeEquals of
    Nothing -> return ""
    Just _ -> parseAttributeValue
  return $ Attribute name value

parseAttributeName :: Parser String String
parseAttributeName = catChars <$> some (noneOf [' ', '"', '\'', '>', '/', '='])

parseAttributeValue :: Parser String String
parseAttributeValue = do
  maybeOpenChar <- optionMaybe $ spaces *> (char '"' <|> char '\'')
  case maybeOpenChar of
    Nothing -> catChars <$> many (noneOf [' ', '>'])
    Just openChar -> do
      value <- catChars <$> many (noneOf [openChar])
      char openChar
      return value

spaces :: Parser String Unit
spaces = void $ many $ char ' '

-- catChars :: forall f. (Foldable f, Functor f) => f Char -> String
catChars :: List Char -> String
catChars = fold <<< map toString

isAlphaNumeric :: Char -> Boolean
isAlphaNumeric c =
  code >= toCharCode '0' && code <= toCharCode '9' ||
  code >= toCharCode 'A' && code <= toCharCode 'Z' ||
  code >= toCharCode 'a' && code <= toCharCode 'z'
  where
  code = toCharCode c

prettyError (ParseError e@{ position = Position pos }) =
  e.message ++ " (" ++ show pos.line ++ "," ++ show pos.column ++ ")"

-- Helpers to write ASTs easier.

element
  :: forall f g. (Foldable f, Foldable g)
  => String -> f Attribute -> g HTML -> HTML
element name attrs children = Element name (toList attrs) (toList children)

voidElement
  :: forall f. (Foldable f)
  => String -> f Attribute -> HTML
voidElement name attrs = VoidElement name (toList attrs)

textNode = TextNode

commentNode = CommentNode
