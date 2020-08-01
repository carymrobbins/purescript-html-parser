module Test.Main where

import Prelude (Unit, discard, show, ($), (/=), (<>))
import Effect (Effect)
import Data.Either (Either(..))
import Data.Foldable (class Foldable)
import Data.List (fromFoldable)
import Text.Parsing.StringParser (ParseError(..))

import Text.HTML.Parser (Attribute(..), HTML, parseHTML)
import Text.HTML.Parser.Array (element, textNode, voidElement)

import Test.Utils (fail, runTest, success)

main :: Effect Unit
main = runTest do
  assertParse "<html></html>"
    [ element "html" [] [] ]

  assertParse "<html><body></body><head></head></html>"
    [ element "html" [] $
        [ element "body" [] []
        , element "head" [] []
        ]
    ]

  assertParse "<br/>"
    [ voidElement "br" [] ]

  assertParse "<p><br/></p>"
    [ element "p" [] [voidElement "br" []] ]

  assertParse "<div></div><span></span>"
    [ element "div" [] []
    , element "span" [] []
    ]

  assertParse "<div>hello<em>big</em>world!</div>"
    [ element "div" []
      [ textNode "hello"
      , element "em" [] [textNode "big"]
      , textNode "world!"
      ]
    ]

  assertParse """<div id="foo" class="bar"></div>"""
    [ element "div"
      [ Attribute "id" "foo"
      , Attribute "class" "bar"
      ] []
    ]

  assertParse """<div id = 'foo' disabled class =bar></div>"""
    [ element "div"
      [ Attribute "id" "foo"
      , Attribute "disabled" ""
      , Attribute "class" """bar"""
      ] []
    ]

  assertParse """<div id="foo"><img src="puppies.gif"/></div>"""
    [ element "div" [Attribute "id" "foo"]
      [ voidElement "img" [Attribute "src" "puppies.gif"] ]
    ]

    -- See if whitespace after attributes affect parsing...
  assertParse """<br class="solid" />"""
    [ voidElement "br" [Attribute "class" "solid"]
    ]

    -- See if unquoted attribute value is parsed properly...
  assertParse """<br class=solid/>"""
    [ voidElement "br" [Attribute "class" "solid"]
    ]

    -- See if spaces around the equals sign affect attribute parsing...
  assertParse """<br class = "solid"/>"""
    [ voidElement "br" [Attribute "class" "solid"]
    ]

    -- See if spaces around the equals sign affect attribute parsing if argument value is not
    --        quoted...

  assertParse """<br class = solid/>"""
    [ voidElement "br" [Attribute "class" "solid"]
    ]

assertParse
  :: forall f. (Foldable f)
  => String -> f HTML -> Effect Unit
assertParse input expected = case parseHTML input of
  Left (ParseError e) ->
    fail $ "Parse error for " <> show input <> "\n  Error: " <> e
  Right actual | actual /= expected' ->
    fail $ "Expected: " <> show expected' <> "\n  Actual: " <> show actual
  _ -> success input
  where
  expected' = fromFoldable expected
