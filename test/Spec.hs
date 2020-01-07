import Test.HUnit
import Control.Applicative
import Data.Function
import Data.Maybe
import Rummikub

validNumberSucc :: String -> String -> String -> Test
validNumberSucc s a b = "Valid number run successor (" ++ s ++ ")"
  ~: (fromMaybe False $ ((liftA2 (<#)) `on` tile) a b) @? c
  where c = a ++ " <# " ++ b

validNumberSuccTests :: Test
validNumberSuccTests = TestList $ TestList <$>
  [ zipWith (validNumberSucc "number-number")
    ["r1", "b3", "o4", "B2", "*10", "r9",  "*11"]
    ["r2", "b4", "o5", "B3", "*11", "*10", "b12"]
  , zipWith (validNumberSucc "repeated jokers")
    ["j", "cj", "dj", "j", "cj",  "dj", "mj", "mj", "mj"]
    ["j", "cj", "dj", "mj", "mj", "mj", "j", "cj",  "dj"]
  , zipWith (validNumberSucc "right jokers")
    ["b2", "b2", "b2", "o12"]
    ["j",  "cj", "dj", "mj" ]
  , zipWith (validNumberSucc "left jokers")
    ["j",  "cj", "dj", "mj"]
    ["b2", "b2", "b2", "o1"]]

invalidNumberSucc :: String -> String -> String -> Test
invalidNumberSucc s a b = "Invalid number run successor (" ++ s ++ ")"
  ~: not (fromMaybe True $ ((liftA2 (<#)) `on` tile) a b) @? c
  where c = a ++ " <# " ++ b

invalidNumberSuccTests :: Test
invalidNumberSuccTests = TestList $ TestList <$>
  [ zipWith (invalidNumberSucc "number-number")
    ["r2", "b4", "o5", "B3", "*11", "*10", "b12"]
    ["r1", "b3", "o4", "B2", "*10", "r9",  "*11"]
  , zipWith (invalidNumberSucc "center jokers")
    ["mj"]
    ["mj"]
  , zipWith (invalidNumberSucc "right jokers")
    ["o12", "o12", "o12", "o11"]
    ["j",   "cj",  "dj",  "dj" ]
  , zipWith (invalidNumberSucc "left jokers")
    ["j",  "cj", "dj", "dj" ]
    ["o1", "o1", "o2", "o2"]]

numberSuccTests :: Test
numberSuccTests = TestList [validNumberSuccTests, invalidNumberSuccTests]

main :: IO ()
main = do (_,s) <- runTestText putTextToShowS numberSuccTests
          putStr $ s ""

