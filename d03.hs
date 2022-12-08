module Main where

import Data.Char (isLower)
import Data.List (foldl')
import qualified Data.Set as S

type Compartment = S.Set Char
type Rucksack = [Compartment]

main :: IO ()
main =
  do
    rucksacks <- loadData "a03.txt"
    print . getTotalPriority $ rucksacks
    
    let groups = chunk 3 . map (\[l, r] -> l `S.union` r) $ rucksacks
    print . getTotalPriority $ groups
  where
    getTotalPriority :: [Rucksack] -> Int
    getTotalPriority = sum . map (prioritize . getSharedItem)

chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n xs = take n xs : chunk n (drop n xs)

getSharedItem :: Rucksack -> Char
getSharedItem = head . S.toList . foldl' S.intersection all
  where
    all :: Compartment
    all = S.fromList $ ['a' .. 'z'] ++ ['A' .. 'Z']

prioritize :: Char -> Int
prioritize c
  | isLower c = fromEnum c - fromEnum 'a' + 1
  | otherwise = fromEnum c - fromEnum 'A' + 27

loadData :: FilePath -> IO [Rucksack]
loadData input =
  do
    ls <- lines <$> readFile input
    return . map parseLine $ ls
  where
    parseLine :: String -> Rucksack
    parseLine line = map S.fromList [a, b]
      where
        mid = length line `div` 2
        (a, b) = splitAt mid line