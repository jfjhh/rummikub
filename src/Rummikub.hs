-- Rummikub.hs
-- Alex Striff

module Rummikub where

import Control.Applicative
import Control.Monad
import Data.Function
import Data.Function.Pointless
import Data.Traversable
import Data.List
import Data.List.Split
import Text.Read

infixr 9 <.>
(<.>) :: Functor f => (a -> b) -> (c -> f a) -> c -> f b
f <.> g = fmap f . g

data Color = Black
           | Red
           | Orange
           | Blue
           | AnyColor
           deriving Show

color :: Char -> Maybe Color
color s
  | s == 'B'  = Just Black
  | s == 'r'  = Just Red
  | s == 'o'  = Just Orange
  | s == 'b'  = Just Blue
  | s == '*'  = Just AnyColor
  | otherwise = Nothing

instance Eq Color where
  Black    == Black    = True
  Red      == Red      = True
  Orange   == Orange   = True
  Blue     == Blue     = True
  AnyColor == _        = True
  _        == AnyColor = True
  _        == _        = False

instance Ord Color where
  compare Black    Red      = LT
  compare Red      Orange   = LT
  compare Orange   Blue     = LT
  compare Black    Black    = EQ
  compare Red      Red      = EQ
  compare Orange   Orange   = EQ
  compare Blue     Blue     = EQ
  compare AnyColor _        = EQ
  compare _        AnyColor = EQ
  compare _        _        = GT

data Tile = NumTile Int Color
          | Joker
          | ColorJoker
          | DoubleJoker
          | MirrorJoker
          deriving (Eq, Show)

tile :: String -> Maybe Tile
tile [] = Nothing
tile s@(c:n)
  | s == "j"  = Just Joker
  | s == "cj" = Just ColorJoker
  | s == "dj" = Just DoubleJoker
  | s == "mj" = Just MirrorJoker
  | otherwise = liftA2 NumTile (readMaybe n :: Maybe Int) (color c)

instance Ord Tile where
  (<=) (NumTile n c) (NumTile m d) = (n <= m) && (c <= d)
  (<=) _ _ = True
  (<) Joker _     = True
  (<) _     Joker = True
  (<) ColorJoker _     = True
  (<) _     ColorJoker = True
  (<) a b = a <= b && a /= b

tileColor (NumTile _ c) = c
tileColor _ = AnyColor

data Run = NumRun [Tile]
         | ColorRun [Tile]
         deriving (Eq, Show)

allRel :: (b -> [(a, a)]) -> (a -> a -> Bool) -> b -> Bool
allRel p r = all (uncurry r) . p

pairs :: [a] -> [(a, a)]
pairs = sequence <=< ap zip tails

allPairs :: (a -> a -> Bool) -> [a] -> Bool
allPairs = allRel pairs

neighbors :: [a] -> [(a, a)]
neighbors = ap zip tail

allNeighbors :: (a -> a -> Bool) -> [a] -> Bool
allNeighbors = allRel neighbors

-- Covers valid DoubleJokers
runEqual :: [Tile] -> [Tile] -> Bool
runEqual = (==) `on` intercalate [Joker, Joker] . splitOn [DoubleJoker]

validMirrorJoker :: [Tile] -> Bool
validMirrorJoker ts = allNeighbors reverseEqual (splitOn [MirrorJoker] ts)
  where reverseEqual x y = runEqual x (reverse y)

validColorJoker :: Run -> Bool
validColorJoker (NumRun []) = True
validColorJoker (NumRun ts) = all ($ subRuns)
  [all $ allNeighbors sameColor, allNeighbors (differentColor `on` head)]
  where subRuns = splitOn [ColorJoker] ts
validColorJoker _ = False

sameColor :: Tile -> Tile -> Bool
sameColor = (==) `on` tileColor

differentColor :: Tile -> Tile -> Bool
differentColor = (/=) `on` tileColor

-- TODO: Check valid run length and endpoints for jokers
-- TODO: Combine all the checks

all' :: [a -> b -> Bool] -> (a -> b -> Bool)
all' = (and .:) . sequence <.> sequence

validRun (NumRun ts) = allNeighbors (all' [(<), sameColor]) ts
validRun (ColorRun ts) = allNeighbors (all' [(<), differentColor]) ts

parseTiles :: String -> Maybe [Tile]
parseTiles = traverse tile . splitOn " "

parseNumRun :: String -> Maybe Run
parseNumRun = fmap NumRun . parseTiles

parseColorRun :: String -> Maybe Run
parseColorRun = fmap ColorRun . parseTiles

