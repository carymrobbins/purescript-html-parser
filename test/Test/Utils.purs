module Test.Utils where

import Prelude
import Control.Apply ((*>))
import Control.Monad.Eff
import Control.Monad.Eff.Console
import Data.Foldable
import Data.Maybe

success :: forall eff. String -> Eff (console :: CONSOLE | eff) Unit
success m = log $ color Green $ "Passed: " ++ m

fail :: forall eff. String -> Eff (console :: CONSOLE | eff) Unit
fail m = log $ color Red $ "Failed: " ++ m

assert :: forall eff. String -> Boolean -> Eff (console :: CONSOLE | eff) Unit
assert m b | b = success m
assert m _     = fail m

assertEq :: forall a eff. (Eq a, Show a) => String -> a -> a -> Eff (console :: CONSOLE | eff) Unit
assertEq m x y | x == y = assert m true
assertEq m x y = assert (m ++ "\n  " ++ extra) false
  where
  shortExtra = show x ++ " /= " ++ show y
  longExtra = show x ++ "\n    /=\n  " ++ show y
  extra = if Data.String.length shortExtra > 100 then longExtra else shortExtra

assertJustWith :: forall a eff.
                  String
               -> Maybe a
               -> (a -> Eff (console :: CONSOLE | eff) Unit)
               -> Eff (console :: CONSOLE | eff) Unit
assertJustWith m (Just a) f = success m *> f a
assertJustWith m Nothing _ = fail m

assertJust :: forall a eff. String -> Maybe a -> Eff (console :: CONSOLE | eff) Unit
assertJust m mA = assertJustWith m mA $ const $ return $ unit

data Color
  = Black
  | White
  | Red
  | Green
  | Yellow
  | Blue
  | Purple

color :: Color -> String -> String
color c s = case c of
  Black  -> "\x1b[30m" ++ s ++ "\x1b[39;49m"
  White  -> "\x1b[37m" ++ s ++ "\x1b[39;49m"
  Red    -> "\x1b[31m" ++ s ++ "\x1b[39;49m"
  Green  -> "\x1b[32m" ++ s ++ "\x1b[39;49m"
  Yellow -> "\x1b[33m" ++ s ++ "\x1b[39;49m"
  Blue   -> "\x1b[34m" ++ s ++ "\x1b[39;49m"
  Purple -> "\x1b[35m" ++ s ++ "\x1b[39;49m"

foreign import exit :: forall eff. Int -> Eff eff Unit
