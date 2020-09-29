module Test.Utils where

import Prelude (class Eq, class Show, Unit, bind, const, discard, pure, show, unit, ($), (*>), (<>), (==), (>))
import Effect (Effect)
import Effect.Class.Console (log)
import Data.Maybe (Maybe(..))
import Data.String (length)

runTest :: Effect Unit -> Effect Unit
runTest test = do
  test
  status <- getExitStatus
  exit status

success :: String -> Effect Unit
success m = log $ color Green $ "Passed: " <> m

fail :: String -> Effect Unit
fail m = do
  setExitStatus 1
  log $ color Red $ "Failed: " <> m

assert :: String -> Boolean -> Effect Unit
assert m b | b = success m
assert m _     = fail m

assertEq
  :: forall a. Eq a => Show a
  => String -> a -> a -> Effect Unit
assertEq m x y | x == y = assert m true
assertEq m x y = assert (m <> "\n  " <> extra) false
  where
  shortExtra = show x <> " /= " <> show y
  longExtra = show x <> "\n    /=\n  " <> show y
  extra = if length shortExtra > 100 then longExtra else shortExtra

assertJustWith :: forall a.
                  String
               -> Maybe a
               -> (a -> Effect Unit)
               -> Effect Unit
assertJustWith m (Just a) f = success m *> f a
assertJustWith m Nothing _ = fail m

assertJust :: forall a. String -> Maybe a -> Effect Unit
assertJust m mA = assertJustWith m mA $ const $ pure $ unit

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
  Black  -> "\x1b[30m" <> s <> "\x1b[39;49m"
  White  -> "\x1b[37m" <> s <> "\x1b[39;49m"
  Red    -> "\x1b[31m" <> s <> "\x1b[39;49m"
  Green  -> "\x1b[32m" <> s <> "\x1b[39;49m"
  Yellow -> "\x1b[33m" <> s <> "\x1b[39;49m"
  Blue   -> "\x1b[34m" <> s <> "\x1b[39;49m"
  Purple -> "\x1b[35m" <> s <> "\x1b[39;49m"

foreign import exit :: Int -> Effect Unit

foreign import setExitStatus :: Int -> Effect Unit

foreign import getExitStatus :: Effect Int
