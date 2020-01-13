-- Rummikub.hs
-- Alex Striff

module Rummikub where

import Prelude hiding
  (Enum, succ, pred, toEnum, fromEnum, enumFrom, enumFromThen, enumFromTo, enumFromThenTo)
import Prelude.SafeEnum
import Control.Applicative
import Control.Arrow
import Control.Monad
import Data.Function
import Data.Maybe
import Data.Function.Pointless
import Data.Traversable
import Data.List
import Data.List.Split
import Text.Read

newtype TileNum = TileNum Int deriving (Eq, Ord)

instance Show TileNum where
  show (TileNum n) = show n

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

data Color = Black | Red | Orange | Blue | AnyColor

instance Show Color where
  show Black    = "B"
  show Red      = "r"
  show Orange   = "o"
  show Blue     = "b"
  show AnyColor = "*"

instance Eq Color where
  Black    /= Black    = False
  Red      /= Red      = False
  Orange   /= Orange   = False
  Blue     /= Blue     = False
  _ /= _ = True

instance Ord Color where
  compare Black    Red      = LT
  compare Red      Orange   = LT
  compare Orange   Blue     = LT
  compare Blue     Black    = LT
  compare Black    Black    = EQ
  compare Red      Red      = EQ
  compare Orange   Orange   = EQ
  compare Blue     Blue     = EQ
  compare AnyColor _        = EQ
  compare _        AnyColor = EQ
  compare _        _        = GT

color :: Char -> Maybe Color
color s
  | s == 'B'  = Just Black
  | s == 'r'  = Just Red
  | s == 'o'  = Just Orange
  | s == 'b'  = Just Blue
  | s == '*'  = Just AnyColor
  | otherwise = Nothing

data Tile = NumTile TileNum Color | Joker

instance Show Tile where
  show (NumTile n c) = show c ++ show n
  show Joker = "j"

instance Eq Tile where
  (NumTile n c) == (NumTile m d) = n == m && c == d
  _ == _ = True -- Comparision with Joker

(<#)   :: Tile -> Tile -> Bool
(NumTile n c) <# (NumTile m d) = succ n == pure m && c == d
Joker         <# (NumTile m _) = minBound < m
(NumTile n _) <# Joker         = n < maxBound
Joker         <# Joker         = True

(<#<)  :: Tile -> Tile -> Bool
(NumTile n c) <#< (NumTile m d) = (succ <=< succ) n == pure m && c == d
Joker         <#< (NumTile m _) = pure minBound < pred m
(NumTile n _) <#< Joker         = succ n < pure maxBound
Joker         <#< Joker         = True

(<##<)  :: Tile -> Tile -> Bool
(NumTile n c) <##< (NumTile m d) = (succ <=< succ <=< succ) n == pure m && c == d
Joker         <##< (NumTile m _) = pure minBound < (pred <=< pred) m
(NumTile n _) <##< Joker         = (succ <=< succ) n < pure maxBound
Joker         <##< Joker         = True

(<@)   :: Tile -> Tile -> Bool
(NumTile n c) <@ (NumTile m d) = c /= d && n == m
_ <@ _ = True -- Comparison with Joker

(<@<)  :: Tile -> Tile -> Bool
(<@<) = (<@)

tileNum :: Tile -> Maybe TileNum
tileNum (NumTile n _) = pure n
tileNum Joker = Nothing

tileColor (NumTile _ c) = c
tileColor _ = AnyColor

tile :: String -> Maybe Tile
tile [] = Nothing
tile s@(c:n)
  | s == "j"  = Just Joker
  | otherwise = liftA2 NumTile (tilenum =<< readMaybe n) (color c)

tiles :: String -> Maybe [Tile]
tiles = traverse tile . splitOn " "

allRel :: (b -> [(a, a)]) -> (a -> a -> Bool) -> b -> Bool
allRel p r = all (uncurry r) . p

dpairs :: [a] -> [(a, a)]
dpairs = sequence <=< ap zip (tail <$> tails)

neighbors :: [a] -> [(a, a)]
neighbors = ap zip tail

acquaintances :: [a] -> [(a, a)]
acquaintances [_] = []
acquaintances xs  = ap zip (tail . tail) xs

randos :: [a] -> [(a, a)]
randos xs
  | length xs < 4 = []
  | otherwise     = ap zip (tail . tail . tail) xs

validNumRun :: [Tile] -> Bool
validNumRun = (. (&)) . flip all $
  [ (3 <=) . length
  , allRel neighbors (<#)
  , allRel acquaintances (<#<)
  , allRel randos (<##<)]

validColorRun :: [Tile] -> Bool
validColorRun = (. (&)) . flip all $
  [ liftA2 (||) (3 ==) (4 ==) . length
  , allRel dpairs ((/=) `on` tileColor)
  , allRel neighbors (==) . mapMaybe tileNum]

validRun :: [Tile] -> Bool
validRun = liftA2 (||) validNumRun validColorRun

friends :: Tile -> Tile -> Bool
friends (NumTile n c) (NumTile m d) =
  (n == m && c /= d) || (c == d && (pred n == pure m || succ n == pure m))
friends Joker _ = True
friends _ Joker = True

type Run    = [Tile]
type Board  = [Run]
type Frag   = Run
type PBoard = ([Frag], Board)

friendSplits :: Run -> Tile -> [(Frag, Frag)]
friendSplits = flip $ liftA2 fmap (flip splitAt) . findIndices . friends

friendSplits' :: Run -> Frag -> ([(Frag, Frag)], [(Frag, Frag)])
friendSplits' = liftA2 (&&&) (. head) (. last) . friendSplits

makeSplitRuns :: Tile -> (Frag, Frag) -> [(Frag, Frag)]
makeSplitRuns t@(NumTile n c) (xs, xs'@(f@(NumTile m d):ys))
  | succ n == pure m = [rightAdd]
  | pure n == succ m = [leftAdd]
  | c /= d = [rightAdd, leftAdd]
  | otherwise = []
  where rightAdd = (xs, t:xs')
        leftAdd  = (xs ++ [f, t], ys)
makeSplitRuns t (xs, xs'@(f:ys)) = [(xs, t:xs'), (xs ++ [f, t], ys)]

makeLeftSplitRuns' :: Frag -> (Frag, Frag) -> [Frag]
makeLeftSplitRuns'  fs (xs, (f@(NumTile m d):ys))    = [xs ++ [f] ++ fs, ys]

makeRightSplitRuns' :: Frag -> (Frag, Frag) -> [Frag]
makeRightSplitRuns' fs (xs, xs'@(f@(NumTile m d):_)) = [xs, fs ++ xs']

fragPlays' :: Run -> Frag -> [[Frag]]
fragPlays' ts fs = (makeLeftSplitRuns' fs <$> ls) ++ (makeRightSplitRuns' fs <$> rs)
  where (ls, rs) = friendSplits' ts fs

fragPlays :: Run -> Frag -> [([Frag], [Run])]
fragPlays = fmap sortRuns .: fragPlays'

addRun :: Run -> Board -> Board
addRun = (:)

addRunFrag :: Run -> [Frag] -> [Frag]
addRunFrag = (:)

sortRuns :: [Run] -> ([Frag], [Run])
sortRuns = foldr (\r (a, b) -> if validRun r then (a, r:b) else (r:a, b)) mempty

addSplitRun :: (Run, Run) -> PBoard -> PBoard
addSplitRun = addRuns . sortRuns . splitRunList
  where splitRunList (a, b) = [a, b]
        addRuns (a, b) (c, d) = (a ++ c, b ++ d)

altTopBot :: Int -> [Int] -- altTopBot 4 == [1, 5, 2, 4, 3]
altTopBot n
  | n > 0 = take n . join . ap (zipWith $ \a b -> [a, b]) reverse $ [1..n]
  | otherwise = []

sumSet :: Int -> [[Int]]
sumSet n = sumSet' n [[]] where
  sumSet' n ss
    | n > 0 = [1..n] >>= \a -> sumSet' (n - a) ((a:) <$> ss)
    | otherwise = ss

fragments :: [a] -> [[[a]]]
fragments xs = reverse $ flip splitPlaces xs <$> sumSet (length xs)

-- playTile :: Tile -> Board -> [Board]
playTile t ts = (:[]) $ fmap (splitWhen (friends t)) ts

