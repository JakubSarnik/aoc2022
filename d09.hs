{-# Language ViewPatterns #-}

module Main where

import Prelude hiding (Left, Right)
import Data.List (find, foldl')
import Data.Maybe (fromJust)
import qualified Data.Set as S
import Helpers

data Move
  = Up
  | Down
  | Left
  | Right
  deriving (Show, Eq)

type Point  = (Int, Int)
type Vector = (Int, Int)

moveBy :: Point -> Vector -> Point
moveBy (r, c) (dr, dc) = (r + dr, c + dc)

manhattan :: Point -> Point -> Vector
manhattan (r1, c1) (r2, c2) = (r2 - r1, c2 - c1)

data State = State {
  knotPositions :: [Point],
  tailVisited   :: S.Set Point
} deriving (Show)

newState :: Int -> State
newState size = State (replicate size (0, 0)) $ S.singleton (0, 0)

moveHead :: State -> Move -> State
moveHead (State ((hr, hc):ps) visited) move = State pieces' visited'
  where
    pieces' = go (hd' : ps)
      where
        hd' =
          case move of
            Up    -> (hr - 1, hc    )
            Down  -> (hr + 1, hc    )
            Left  -> (hr,     hc - 1)
            Right -> (hr,     hc + 1)

    visited' = S.insert (last pieces') visited

    go :: [Point] -> [Point]
    go [x] = [x]
    go (hd'@(hr', hc') : tl@(tr, tc) : rest) = hd' : go (tl' : rest)
      where
        tl' =
          if touchesHead tl then
            tl
          else
            fromJust . find touchesHead . map (moveBy tl) $ dirs

        dirs =
          if hr' == tr || hc' == tc then
            stNeighborhood
          else
            diagNeighborhood

        touchesHead :: Point -> Bool
        touchesHead pt = manhattan pt hd' `elem` neighborhood

        diagNeighborhood = [(r, c) | r <- [1, -1], c <- [1, -1]]
        stNeighborhood   = [(1, 0), (-1, 0), (0, 1), (0, -1)]
        neighborhood     = [(r, c) | r <- [-1..1], c <- [-1..1]]

main :: IO ()
main =
  do
    moves <- loadData "a09.txt"

    let
      run :: State -> State
      run init = foldl' moveHead init moves

      vis :: State -> Int
      vis = length . tailVisited

    print . vis . run . newState $ 2
    print . vis . run . newState $ 10

loadData :: FilePath -> IO [Move]
loadData input = concatMap lineToMoves <$> readLines input
  where
    lineToMoves :: String -> [Move]
    lineToMoves (words -> [m, n]) = replicate (read n) $ parseMove m

    parseMove :: String -> Move
    parseMove "U" = Up
    parseMove "D" = Down
    parseMove "L" = Left
    parseMove "R" = Right