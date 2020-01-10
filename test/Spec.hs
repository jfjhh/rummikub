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
    ["r1", "b3", "o4", "B2"]
    ["r2", "b4", "o5", "B3"]
  , zipWith (validNumberPair "repeated jokers")
    ["j"]
    ["j"]
  , zipWith (validNumberPair "right jokers")
    ["b6"]
    ["j"]
  , zipWith (validNumberPair "left jokers")
    ["j"]
    ["b6"]]

invalidNumberPair :: String -> String -> String -> Test
invalidNumberPair s a b = "Invalid number run successor (" ++ s ++ ")"
  ~: not (fromMaybe True $ ((liftA2 (<#)) `on` tile) a b) @? c
  where c = a ++ " <# " ++ b

invalidNumberPairTests :: Test
invalidNumberPairTests = TestList $ TestList <$>
  [ zipWith (invalidNumberPair "number-number")
    ["r2", "b4", "o5", "B3"]
    ["r1", "b3", "o4", "B2"]
  , zipWith (invalidNumberPair "right jokers")
    ["o12"]
    ["j"]
  , zipWith (invalidNumberPair "left jokers")
    ["j"]
    ["o1"]]

numberPairTests :: Test
numberPairTests = TestList [validNumberPairTests, invalidNumberPairTests]

main :: IO ()
main = do (_,s) <- runTestText putTextToShowS numberPairTests
          putStr $ s ""

