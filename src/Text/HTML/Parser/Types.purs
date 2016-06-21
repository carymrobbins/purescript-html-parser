module Text.HTML.Parser.Types where

import Prelude
import Data.List

data HTML
  = Element String (List Attribute) (List HTML)
  | VoidElement String (List Attribute)
  | TextNode String
  | CommentNode String

instance eqHTML :: Eq HTML where
  eq (Element name1 attrs1 children1) (Element name2 attrs2 children2) =
    name1 == name2 && attrs1 == attrs2 && children1 == children2
  eq (Element _ _ _) _ = false

  eq (VoidElement name1 attrs1) (VoidElement name2 attrs2) =
    name1 == name2 && attrs1 == attrs2
  eq (VoidElement _ _) _ = false

  eq (TextNode text1) (TextNode text2) = text1 == text2
  eq (TextNode _) _ = false

  eq (CommentNode text1) (CommentNode text2) = text1 == text2
  eq (CommentNode _) _ = false

instance showHTML :: Show HTML where
  show (Element name attrs children) =
    "Element " <> show name <> " (" <> show attrs <> ") (" <> show children <> ")"
  show (VoidElement name attrs) =
    "VoidElement " <> show name <> " (" <> show attrs <> ")"
  show (TextNode text) = "TextNode " <> show text
  show (CommentNode text) = "CommentNode " <> show text

data Attribute = Attribute String String

instance eqAttribute :: Eq Attribute where
  eq (Attribute k1 v1) (Attribute k2 v2) = k1 == k2 && v1 == v2

instance showAttribute :: Show Attribute where
  show (Attribute k v) = "Attribute " <> show k <> " " <> show v

