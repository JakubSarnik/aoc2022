{-# Language ScopedTypeVariables #-}

module Main where

data Play = Rock | Paper | Scissors
  deriving (Show, Eq, Enum)

data Result = Won | Lost | Draw
  deriving (Show, Eq)

main :: IO ()
main =
  do
    rounds1 <- loadData charToPlay "a02.txt"
    print . score $ rounds1

    rounds2 <- loadData charToResult "a02.txt"
    print . score. map resultToMove $ rounds2
  where
    score :: [(Play, Play)] -> Integer
    score = sum . map scoreRound

loop :: Int -> Play -> Play
loop n x = toEnum $ (fromEnum x + n) `mod` 3

scoreRound :: (Play, Play) -> Integer
scoreRound (opponent, player) = shape player + result player opponent
  where
    shape :: Play -> Integer
    shape p = (fromIntegral . fromEnum $ p) + 1

    result :: Play -> Play -> Integer
    result p o
      | loop 1    p == o = 0
      | loop 0    p == o = 3
      | loop (-1) p == o = 6

resultToMove :: (Play, Result) -> (Play, Play)
resultToMove (x, r) = (x, r')
  where
    r' =
      case r of
        Won  -> loop 1    x
        Lost -> loop (-1) x
        Draw -> loop 0    x

charToPlay :: String -> Play
charToPlay c
  | c `elem` ["A", "X"] = Rock
  | c `elem` ["B", "Y"] = Paper
  | c `elem` ["C", "Z"] = Scissors
  | otherwise           = error "charToPlay: no parse"

charToResult :: String -> Result
charToResult "X" = Lost
charToResult "Y" = Draw
charToResult "Z" = Won
charToResult _   = error "charToResult: no parse"

loadData :: forall a. (String -> a) -> FilePath -> IO [(Play, a)]
loadData f input =
  do
    ls <- lines <$> readFile input
    return . map parseLine $ ls
  where
    parseLine :: String -> (Play, a)
    parseLine l = (charToPlay x, f y)
      where
        [x, y] = words l