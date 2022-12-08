module Main where

import Helpers
import Data.List.Split (splitOn)

type Interval = (Integer, Integer)
type Assignment = (Interval, Interval)

main :: IO ()
main =
  do
    assignments <- loadData "a04.txt"
    print . strategy subsumed $ assignments
    print . strategy overlaps $ assignments
  where
    strategy :: (Assignment -> Bool) -> [Assignment] -> Int
    strategy s = length . filter s

subsumed :: Assignment -> Bool
subsumed (i1, i2) = subsumes i1 i2 || subsumes i2 i1
  where
    subsumes :: Interval -> Interval -> Bool
    subsumes (a, b) (a', b') = a <= a' && b' <= b

overlaps :: Assignment -> Bool
overlaps ((a, b), (c, d)) = c <= b && a <= d

loadData :: FilePath -> IO [Assignment]
loadData input = map parseLine . lines <$> readFile input
  where
    parseLine :: String -> Assignment
    parseLine = listToPair . map rangeToInterval . splitOn ","

    rangeToInterval :: String -> Interval
    rangeToInterval = listToPair . map read . splitOn "-"