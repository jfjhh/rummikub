-- Rummikub.hs
-- Alex Striff

module Rummikub where

import Prelude hiding
  (Enum, succ, pred, toEnum, fromEnum, enumFrom, enumFromThen, enumFromTo, enumFromThenTo)
import Prelude.SafeEnum
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

liftAA2 :: (Applicative f1, Applicative f2)
        => (a -> b -> c) -> f1 (f2 a) -> f1 (f2 b) -> f1 (f2 c)
liftAA2 = liftA2 . liftA2

newtype TileNum = TileNum Int deriving (Eq, Ord, Show)

instance Bounded TileNum where
  minBound = TileNum 1
  maxBound = TileNum 12

instance DownwardEnum TileNum where
  pred t@(TileNum n)
    | minBound < t && t <= maxBound = TileNum <$> pred n
    | otherwise = Nothing
  precedes (TileNum n) (TileNum m) = minBound <= n && n < m && m <= maxBound

instance UpwardEnum TileNum where
  succ t@(TileNum n)
    | minBound <= t && t < maxBound = TileNum <$> succ n
    | otherwise = Nothing
  succeeds (TileNum n) (TileNum m) = maxBound >= n && n > m && m >= minBound

instance Enum TileNum where
  toEnum n
    | minBound <= tn && tn <= maxBound = Just tn
    | otherwise = Nothing
    where tn = TileNum n
  fromEnum (TileNum n) = Just n

tilenum :: Int -> Maybe TileNum
tilenum = toEnum

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

instance DownwardEnum Color where
  pred Black    = Just Blue
  pred Red      = Just Black
  pred Orange   = Just Red
  pred Blue     = Just Orange
  pred AnyColor = Just AnyColor
  precedes AnyColor _ = True
  precedes _ AnyColor = True
  precedes x y = x /= y

data Tile = NumTile TileNum Color
          | Joker
          | ColorJoker
          | DoubleJoker
          | MirrorJoker
          deriving Show

instance Eq Tile where
  MirrorJoker   == MirrorJoker = True
  Joker         == _           = True
  ColorJoker    == _           = True
  DoubleJoker   == _           = True
  MirrorJoker   == _           = False
  _             == Joker       = True
  _             == ColorJoker  = True
  _             == DoubleJoker = True
  _             == MirrorJoker = False
  (NumTile n c) == (NumTile m d) = n   == m && c == d

instance Ord Tile where
  (<=) (NumTile n c) (NumTile m d) = n <= m && c <= d
  (<=) _ _ = True
  (<) Joker _     = True
  (<) _     Joker = True
  (<) ColorJoker _     = True
  (<) _     ColorJoker = True
  (<) a b = a <= b && a /= b

instance DownwardEnum Tile where
  pred (NumTile n c) = NumTile n <$> pred c
  pred joker = Just joker
  precedes (NumTile n c) (NumTile m d) = n == m && c `precedes` d
  precedes _ _ = True

instance UpwardEnum Tile where
  succ (NumTile n c) = flip NumTile c <$> succ n
  succ joker = Just joker
  succeeds (NumTile n c) (NumTile m d) = c == d && n `succeeds` m
  succeeds _ _ = True

(<#)   :: Tile -> Tile -> Bool
(<#<)  :: Tile -> Tile -> Bool
(<##<) :: Tile -> Tile -> Bool
(<#)   = (==) $::  succ                    ~> return ~> id
(<#<)  = (==) $:: (succ <=< succ)          ~> return ~> id
(<##<) = (==) $:: (succ <=< succ <=< succ) ~> return ~> id

(<@)   :: Tile -> Tile -> Bool
(<@<)  :: Tile -> Tile -> Bool
(<@@<) :: Tile -> Tile -> Bool
(<@)   = (==) $:: return ~>  pred                    ~> id
(<@<)  = (==) $:: return ~> (pred <=< pred)          ~> id
(<@@<) = (==) $:: return ~> (pred <=< pred <=< pred) ~> id

tile :: String -> Maybe Tile
tile [] = Nothing
tile s@(c:n)
  | s == "j"  = Just Joker
  | s == "cj" = Just ColorJoker
  | s == "dj" = Just DoubleJoker
  | s == "mj" = Just MirrorJoker
  | otherwise = liftA2 NumTile (tilenum =<< readMaybe n) (color c)

tileColor (NumTile _ c) = c
tileColor _ = AnyColor

data Run = NumRun [Tile]
         | ColorRun [Tile]
         deriving Show

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

acquaintances :: [a] -> [(a, a)]
acquaintances [_] = []
acquaintances xs  = ap zip (tail . tail) xs

allAcquaintances :: (a -> a -> Bool) -> [a] -> Bool
allAcquaintances = allRel acquaintances

triples :: [a] -> [((a, a), a)]
triples = (liftA2 zip . ap zip) tail (tail . tail)

allTriples :: (a -> a -> Bool) -> [a] -> Bool
allTriples = liftAA2 (&&) allNeighbors allAcquaintances

duplicateDoubleJokers :: [Tile] -> [Tile]
duplicateDoubleJokers = intercalate [Joker, Joker] . splitOn [DoubleJoker]

runEqual :: [Tile] -> [Tile] -> Bool
runEqual = (==) `on` duplicateDoubleJokers

instance Eq Run where
  (NumRun x)   == (NumRun y)   = x `runEqual` y
  (ColorRun x) == (ColorRun y) = x `runEqual` y
  _ == _ = False

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
-- FIXME: The number run `b1 j b2` tests valid since Ord Tile is insufficient.

all' :: [a -> b -> Bool] -> (a -> b -> Bool)
all' = (and .:) . sequence <.> sequence

validNumRun x y z
  | otherwise = x `nn` y && y `nn` z && x `na` z
  where nn = liftAA2 (&&) (<) sameColor
        na = liftAA2 (&&) (<#<) sameColor

validRun (NumRun ts)   = duplicateDoubleJokers ts & liftA2 (&&)
                         (allNeighbors     $ liftAA2 (&&) (<)   sameColor)
                         (allAcquaintances $ liftAA2 (&&) (<#<) sameColor)
validRun (ColorRun ts) = allTriples (all' [(<), differentColor]) ts

parseTiles :: String -> Maybe [Tile]
parseTiles = traverse tile . splitOn " "

parseNumRun :: String -> Maybe Run
parseNumRun = fmap NumRun . parseTiles

parseColorRun :: String -> Maybe Run
parseColorRun = fmap ColorRun . parseTiles

