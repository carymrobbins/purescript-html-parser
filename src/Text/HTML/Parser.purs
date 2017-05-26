module Text.HTML.Parser
  ( module Text.HTML.Parser.Types
  , parseHTML
  ) where

import Text.HTML.Parser.Internal as I
import Text.HTML.Parser.Types (Attribute(..), HTML(..), ParseResult)

parseHTML :: String -> ParseResult
parseHTML = I.parseHTML
