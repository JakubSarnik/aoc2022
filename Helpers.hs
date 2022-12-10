module Helpers
  (listToPair, slidingWindow, readLines)
where

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