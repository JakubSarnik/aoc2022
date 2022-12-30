{-# Language ViewPatterns #-}

module Main where

import Helpers
import Data.List.Split (chunksOf)

data Instruction
  = NoOp
  | AddX Integer
  deriving (Show, Eq)

main :: IO ()
main =
  do
    prog <- loadData "a10.txt"
    print $ sampleRun prog

    putStrLn $ drawLines prog

traceProgram :: Integer -> [Instruction] -> [Integer]
traceProgram x []            = []
traceProgram x (NoOp:is)     = x : traceProgram x is
traceProgram x ((AddX d):is) = x : x' : traceProgram x' is
  where
    x' = x + d

sampleRun :: [Instruction] -> Integer
sampleRun prog = sum . map (\(x, i) -> x * i) $ relevant
  where
    relevant = filter (\(_, i) -> 40 `divides` i - 20) . zip (traceProgram 1 prog) $ [2 ..]

drawLines :: [Instruction] -> String
drawLines prog = unlines $ map (goLine 0) chunks
  where
    goLine :: Integer -> [Integer] -> String
    goLine _     []           = ""
    goLine drawX (spriteX:xs) =
      (if overlaps then
        '#'
       else
         '.') : goLine (drawX + 1) xs
      where
        overlaps = abs (spriteX - drawX) < 2
    
    -- Ugh
    trace = 1 : (init $ traceProgram 1 prog)
    chunks = chunksOf 40 trace

loadData :: FilePath -> IO [Instruction]
loadData input = map parseInst <$> readLines input
  where
    parseInst :: String -> Instruction
    parseInst "noop"            = NoOp
    parseInst (words -> [_, n]) = AddX $ read n
