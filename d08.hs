{-# Language TypeApplications, ScopedTypeVariables #-}

module Main where

import Data.Ix
import Data.Array
import Helpers

data Point = Point { row :: Int, column :: Int }
  deriving (Eq, Ord, Show, Ix)

move :: Point -> Point -> Point
move (Point r c) (Point dr dc) = Point (r + dr) (c + dc)

dirs :: [Point]
dirs = [Point 1 0, Point (-1) 0, Point 0 1, Point 0 (-1)]

type Matrix = Array Point Integer

main :: IO ()
main =
  do
    forest <- loadData "a08.txt"
    let visibility = imap (visible forest) forest
    print . sum . fmap fromEnum $ visibility

    let scenicScores = imap (getScenicScore forest) forest
    print . maximum $ scenicScores

visible :: Matrix -> Point -> Integer -> Bool
visible m pt height = any (\v -> foldSweep m pt v accShorter True) dirs
  where
    accShorter :: Bool -> Point -> Integer -> Bool
    accShorter acc p ht = (p == pt) || (acc && ht < height)

getScenicScore :: Matrix -> Point -> Integer -> Integer
getScenicScore m pt height = product . map fst . map (\v -> foldSweep m pt v increaseScore (0, True)) $ dirs
  where
    increaseScore :: (Integer, Bool) -> Point -> Integer -> (Integer, Bool)
    increaseScore (acc, True) p ht = (acc', p == pt || ht < height)
      where
        acc' = acc + if p /= pt then 1 else 0
    increaseScore x _ _ = x

foldSweep :: forall a. Matrix -> Point -> Point -> (a -> Point -> Integer -> a) -> a -> a
foldSweep m s v f i = go s i
  where
    go :: Point -> a -> a
    go pt acc
      | bounds m `inRange` pt = go (move pt v) (f acc pt (m ! pt))
      | otherwise             = acc

loadData :: FilePath -> IO Matrix
loadData input =
  do
    ls <- readLines input

    let matrix = map (map (\c -> read @Integer [c])) ls

    let rows = length matrix
    let cols = length $ matrix !! 0

    pure . listArray (Point 1 1, Point rows cols) $ concat matrix