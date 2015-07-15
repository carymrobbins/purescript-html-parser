module Test.Main where

import Prelude
import Control.Monad.Eff
import Control.Monad.Eff.Console
import Data.Either
import Data.Foldable
import Data.List

import Text.HTML.Parser
import Test.Utils

main = do
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

  assertParse """<div id = 'foo' disabled class =bar"<></div>"""
    [ element "div"
      [ Attribute "id" "foo"
      , Attribute "disabled" ""
      , Attribute "class" """bar"<"""
      ] []
    ]

  assertParse """<div id="foo"><img src="puppies.gif"/></div>"""
    [ element "div" [Attribute "id" "foo"]
      [ voidElement "img" [Attribute "src" "puppies.gif"] ]
    ]

assertParse
  :: forall eff f. (Foldable f)
  => String -> f HTML -> Eff (console :: CONSOLE | eff) Unit
assertParse input expected = case parseHTML input of
  Left e ->
    fail $ "Parse error for " ++ show input ++ "\n  Error: " ++ prettyError e
  Right actual | actual /= expected' ->
    fail $ "Expected: " ++ show expected' ++ "\n  Actual: " ++ show actual
  _ -> success input
  where
  expected' = toList expected

