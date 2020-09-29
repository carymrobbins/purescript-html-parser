module Text.HTML.Parser.Combinators where

import Prelude

import Control.Alt ((<|>))
import Data.Char (toCharCode)
import Data.Foldable (class Foldable, foldMap)
import Data.String.CodeUnits (singleton)
import Text.Parsing.StringParser (Parser, fail, try)

-- | Fail if the specified parser matches.
notFollowedBy :: forall a. Parser a -> Parser Unit
notFollowedBy p = try $ (try p *> fail "Negated parser succeeded") <|> pure unit

isAlphaNumeric :: Char -> Boolean
isAlphaNumeric c =
  code >= toCharCode '0' && code <= toCharCode '9' ||
  code >= toCharCode 'A' && code <= toCharCode 'Z' ||
  code >= toCharCode 'a' && code <= toCharCode 'z'
  where
  code = toCharCode c

fromChars :: forall f. Foldable f => f Char -> String
fromChars = foldMap singleton
