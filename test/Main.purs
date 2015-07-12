module Test.Main where

import Prelude
import Control.Monad.Eff
import Control.Monad.Eff.Console
import Data.Either
import Data.List

import Text.HTML.Parser
import Test.Utils

main = do
  assertParse "<html></html>" $
    Element "html" Nil Nil
  assertParse "<html><body></body><head></head></html>" $
    Element "html" Nil (Cons
      (Element "body" Nil Nil) (Cons
      (Element "head" Nil Nil) Nil))
  assertParse "<br/>" $
    VoidElement "br" Nil

assertParse :: forall eff. String -> HTML -> Eff (console :: CONSOLE | eff) Unit
assertParse input expected = case parseHTML input of
  Left e ->
    fail $ "Parse error for " ++ show input ++ "\n  Error: " ++ prettyError e
  Right actual | actual /= expected ->
    fail $ "Expected: " ++ show expected ++ "\n  Actual: " ++ show actual
  _ -> success input
