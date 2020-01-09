import Test.HUnit
import Control.Applicative
import Data.Function
import Data.Maybe
import Rummikub

validNumberPair :: String -> String -> String -> Test
validNumberPair s a b = "Valid number run successor (" ++ s ++ ")"
  ~: (fromMaybe False $ ((liftA2 (<#)) `on` tile) a b) @? c
  where c = a ++ " <# " ++ b

validNumberPairTests :: Test
validNumberPairTests = TestList $ TestList <$>
  [ zipWith (validNumberPair "number-number")
    ["r1", "b3", "o4", "B2", "*10", "r9",  "*11"]
    ["r2", "b4", "o5", "B3", "*11", "*10", "b12"]
  , zipWith (validNumberPair "repeated jokers")
    ["j", "cj", "dj", "j", "cj",  "dj", "mj", "mj", "mj"]
    ["j", "cj", "dj", "mj", "mj", "mj", "j", "cj",  "dj"]
  , zipWith (validNumberPair "right jokers")
    ["b6", "b6", "b6", "o12"]
    ["j",  "cj", "dj", "mj" ]
  , zipWith (validNumberPair "left jokers")
    ["j",  "cj", "dj", "mj"]
    ["b6", "b6", "b6", "o1"]]

invalidNumberPair :: String -> String -> String -> Test
invalidNumberPair s a b = "Invalid number run successor (" ++ s ++ ")"
  ~: not (fromMaybe True $ ((liftA2 (<#)) `on` tile) a b) @? c
  where c = a ++ " <# " ++ b

invalidNumberPairTests :: Test
invalidNumberPairTests = TestList $ TestList <$>
  [ zipWith (invalidNumberPair "number-number")
    ["r2", "b4", "o5", "B3", "*11", "*10", "b12"]
    ["r1", "b3", "o4", "B2", "*10", "r9",  "*11"]
  , zipWith (invalidNumberPair "center jokers")
    ["mj"]
    ["mj"]
  , zipWith (invalidNumberPair "right jokers")
    ["o12", "o12", "o12", "o11"]
    ["j",   "cj",  "dj",  "dj" ]
  , zipWith (invalidNumberPair "left jokers")
    ["j",  "cj", "dj", "dj" ]
    ["o1", "o1", "o1", "o2"]]

numberPairTests :: Test
numberPairTests = TestList [validNumberPairTests, invalidNumberPairTests]

main :: IO ()
main = do (_,s) <- runTestText putTextToShowS numberPairTests
          putStr $ s ""

