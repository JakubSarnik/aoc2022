{-# Language ScopedTypeVariables #-}

module Helpers
  (listToPair, slidingWindow, readLines, imap, divides)
where

import Data.Array.IArray

listToPair :: [a] -> (a, a)
listToPair [x, y] = (x, y)
listToPair _      = error "listToPair: not a pair list"

slidingWindow :: Int -> [a] -> [[a]]
slidingWindow _ [] = []
slidingWindow n xs =
  if length peek < n then
    []
  else
    peek : slidingWindow n (drop 1 xs) 
  where
    peek = take n xs

readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile

imap :: forall a i e e'. (IArray a e, IArray a e', IArray a (i, e), Ix i)
     => (i -> e -> e') -> a i e -> a i e'
imap f = amap (uncurry f) . zipWithIndices
  where
    zipWithIndices :: a i e -> a i (i, e)
    zipWithIndices arr = listArray (bounds arr) . assocs $ arr

infixl 1 `divides`

divides :: (Integral a) => a -> a -> Bool
divides m n = n `mod` m == 0