module Main where

import Data.List.Split (splitOn)
import Data.List (transpose, foldl')
import Data.Char (isAlpha)
import Data.Array


type Crate = Char
type Stack = [Crate] -- Top is first
type Ship  = Array Int Stack
type Move  = (Int, Int, Int) -- How many, from where, to where
type Plan  = (Ship, [Move])

main :: IO ()
main =
  do
    (s, moves) <- loadData "a05.txt"

    let done1 = foldl' (performMove reverse) s moves
    print . getFirsts $ done1

    let done2 = foldl' (performMove id) s moves
    print . getFirsts $ done2
  where
    getFirsts :: Ship -> String
    getFirsts = map head . elems

performMove :: ([Crate] -> [Crate]) -> Ship -> Move -> Ship
performMove strategy s (what, from, to) = s // [(from, rest), (to, newTo)]
  where
    (moved, rest) = splitAt what $ s ! from
    newTo = strategy moved ++ (s ! to)

loadData :: FilePath -> IO Plan
loadData input =
  do
    ls <- lines <$> readFile input
    let [crates, moves] = splitOn [""] ls
    let stacks = parseShip . init $ crates

    pure (listArray (1, length stacks) stacks, map parseMove moves)
  where
    parseShip :: [String] -> [Stack]
    parseShip = nonEmpty . map (filter isAlpha) . transpose

    parseMove :: String -> Move
    parseMove line = (read what, read from, read to)
      where
        [_, what, _, from, _, to] = words line

    nonEmpty :: [String] -> [String]
    nonEmpty = filter (not . null)