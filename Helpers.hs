module Helpers
  (listToPair)
where

listToPair :: [a] -> (a, a)
listToPair [x, y] = (x, y)
listToPair _      = error "listToPair: not a pair list"