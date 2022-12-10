module Main where

import Data.List (nub)
import Helpers

main :: IO ()
main =
  do
    transmission <- readFile "a06.txt"
    print . findMarker 4 $ transmission
    print . findMarker 14 $ transmission

findMarker :: Int -> String -> Int
findMarker size = (+ size) . length . takeWhile (not . markerStart) . slidingWindow size
  where
    markerStart :: String -> Bool
    markerStart s = nub s == s