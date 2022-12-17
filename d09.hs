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
  headPos     :: Point, 
  tailPos     :: Point,
  tailVisited :: S.Set Point
} deriving (Show)

drawState :: Int -> State -> String
drawState side state = unlines . reverse $ [drawRow $ row | row <- [0 .. (side - 1)]]
  where
    drawRow :: Int -> String
    drawRow r = [getChar r col | col <- [0 .. (side - 1)]]
      where
        getChar :: Int -> Int -> Char
        getChar row col
          | (row, col) == headPos state = 'H'
          | (row, col) == tailPos state = 'T'
          | otherwise                   = '.'

moveHead :: State -> Move -> State
moveHead (State (hr, hc) tl@(tr, tc) visited) move = State hd' tl' visited'
  where
    hd'@(hr', hc') =
      case move of
        Up    -> (hr - 1, hc    )
        Down  -> (hr + 1, hc    )
        Left  -> (hr,     hc - 1)
        Right -> (hr,     hc + 1)
    
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

    visited' = S.insert tl' visited

    touchesHead :: Point -> Bool
    touchesHead pt = manhattan pt hd' `elem` neighborhood

    neighborhood     = [(r, c) | r <- [(-1)..1], c <- [(-1)..1]]
    diagNeighborhood = [(r, c) | r <- [1, -1], c <- [1, -1]]
    stNeighborhood   = [(1, 0), (-1, 0), (0, 1), (0, -1)]

main :: IO ()
main =
  do
    moves <- loadData "a09.txt"
    let initial = State (0, 0) (0, 0) $ S.singleton (0, 0)
    let endState = foldl' moveHead initial moves

    print . length . tailVisited $ endState

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