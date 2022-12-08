module Main where

import Control.Applicative (liftA2)

main :: IO ()
main =
  do
    groups <- loadData "a01.txt"
    let calories = map sum groups
    print . maximum $ calories
    
    let (a, b, c) = maximum3 calories
    print $ a + b + c

maximum3 :: [Integer] -> (Integer, Integer, Integer)
maximum3 xs = (a, b, c)
  where
    a = maximum xs
    b = maximum . filter (/= a) $ xs
    c = maximum . filter (liftA2 (&&) (/= a) (/= b)) $ xs

loadData :: FilePath -> IO [[Integer]]
loadData input =
  do
    ls <- lines <$> readFile input
    let groups = groupAtEmpty ls
    return $ map (map read) groups
  where
    groupAtEmpty :: [String] -> [[String]]
    groupAtEmpty [] = []
    groupAtEmpty xs = group : groupAtEmpty (drop 1 rest)
      where
        (group, rest) = span (not . null) xs