module Main where

import Data.List (foldl', minimumBy)
import Data.Function (on)
import Data.Sequence (Seq(..), singleton, (|>))
import qualified Data.Map.Strict as M
import Data.List.Split (split, whenElt, dropInitBlank)
import Helpers

data Item
  = File String Integer                          -- Files store name and size
  | Directory String Integer (M.Map String Item) -- Directory maps from names to items
  deriving (Show)

getName :: Item -> String
getName (File      name _  ) = name
getName (Directory name _ _) = name

getSize :: Item -> Integer
getSize (File      _ size  ) = size
getSize (Directory _ size _) = size

listDirs :: Item -> [(String, Integer)]
listDirs (File      _    _      ) = []
listDirs (Directory name size ds) = (name, size) : (M.elems ds >>= listDirs)

data Command
  = Cd String
  | Ls
  deriving (Show)

-- Working path and filesystem
type WorkingPath = Seq String
type State = (WorkingPath, Item)

-- Command and its output
type TracePoint = (Command, [Item])
type Trace = [TracePoint]

initial :: State
initial = (singleton "/", Directory "/" 0 M.empty)

mergeTrees :: Item -> WorkingPath -> Item -> Item
mergeTrees _    Empty         _       = error "mergeTrees: empty path"
mergeTrees root (_ :<| path)  subtree = go root path
  where
    go :: Item -> WorkingPath -> Item
    go (File      _    _      ) _     = error "mergeTrees: move into a file"
    go (Directory name size ds) Empty = Directory name size' ds'
      where
        size' = size + getSize subtree
        ds'   = M.insert (getName subtree) subtree ds
    go (Directory name _ ds) (next :<| rest) = Directory name size' ds'
      where
        ds'   = M.mapWithKey updateSubdirs ds
        size' = sum . map getSize . M.elems $ ds'

        updateSubdirs :: String -> Item -> Item
        updateSubdirs n item
          | n == next = go subdir rest
          | otherwise    = item

        subdir =
          case M.lookup next ds of
            Just x  -> x
            Nothing -> error "mergeTrees: move into a missing directory"

main :: IO ()
main =
  do
    trace <- loadData "a07.txt"
    let (_, fs) = foldl' replayTracePoint initial trace
    let dirs = listDirs fs

    print . sum . map snd . filter (\(_, size) -> size <= 100000) $ dirs

    let unused = 70000000 - getSize fs
    let bigEnough (_, size) = unused + size >= 30000000

    print . snd . minimumBy (compare `on` snd) . filter bigEnough $ dirs


replayTracePoint :: State -> TracePoint -> State
replayTracePoint (wd, fs) (cmd, res) = (changeDir cmd, changeFs res)
  where
    changeDir :: Command -> WorkingPath
    changeDir Ls        = wd
    changeDir (Cd "/")  = singleton "/"
    changeDir (Cd "..") = let (rest :|> _) = wd in rest
    changeDir (Cd dir)  = wd |> dir

    changeFs :: [Item] -> Item
    changeFs = foldl' (\fs' item -> mergeTrees fs' wd item) fs


loadData :: FilePath -> IO Trace
loadData input = breakUp <$> readLines input
  where
    breakUp :: [String] -> Trace
    breakUp cmds = toTrace broken
      where
        broken = split (dropInitBlank $ whenElt (\line -> head line == '$')) cmds

        toTrace :: [[String]] -> Trace
        toTrace [] = []
        toTrace ([x]:y:xs) = (readCommand x, map readItem y) : toTrace xs

        readCommand :: String -> Command
        readCommand "$ ls" = Ls
        readCommand cd     = let [_, _, dir] = words cd in Cd dir

        readItem :: String -> Item
        readItem output =
          let
            [spec, name] = words output
          in
            if spec == "dir" then
              Directory name 0 M.empty
            else
              File name $ read spec