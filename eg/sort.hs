-- sort.hs: conjuring a sort function
--
-- Copyright (C) 2021-2025 Rudy Matela
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
import Conjure
import Data.List (insert)

sort' :: [Int] -> [Int]
sort' []       =  []
sort' [0]      =  [0]
sort' [1]      =  [1]
sort' [0,2]    =  [0,2]
sort' [1,0]    =  [0,1]
sort' [0,1,2]  =  [0,1,2]
sort' [2,1,0]  =  [0,1,2]
sort' [1,0,1]  =  [0,1,1]
sort' [0,1,0,1]  =  [0,0,1,1]

insert' :: Int -> [Int] -> [Int]
insert' 0 []  =  [0]
insert' 0 [1,2]  =  [0,1,2]
insert' 1 [0,2]  =  [0,1,2]
insert' 2 [0,1]  =  [0,1,2]

merge' :: [Int] -> [Int] -> [Int]
merge' [] []  =  []
merge' [0] []  =  [0]
merge' [] [0]  =  [0]
merge' [0] [1]  =  [0,1]
merge' [1] [0]  =  [0,1]
merge' [1] [0,2]  =  [0,1,2]
merge' [0,2] [1]  =  [0,1,2]
merge' [0,1] [0,1]  =  [0,0,1,1]
merge' [0,1] [2,3]  =  [0,1,2,3]
merge' [0,2] [1,3]  =  [0,1,2,3]
merge' [0,1] [1,2]  =  [0,1,1,2]
merge' [1,2] [0,1]  =  [0,1,1,2]

main :: IO ()
main = do
  -- an insert function
  conjure "insert" insert'
    [ fun "[]" ([] :: [Int])
    , fun ":" ((:) :: Int -> [Int] -> [Int])
    , fun "<=" ((<=) :: Int -> Int -> Bool)
    , guard
    ]

  -- recursive insertion sort
  conjure "sort" sort'
    [ con ([] :: [Int])
    , fun "insert" (insert :: Int -> [Int] -> [Int])
    ]

  -- folding insertion sort
  conjure "sort" sort'
    [ con ([] :: [Int])
    , fun "insert" (insert :: Int -> [Int] -> [Int])
    , fun "foldr" (foldr :: (Int -> [Int] -> [Int]) -> [Int] -> [Int] -> [Int])
    ]

  -- found!  candidate #353593 @ size 22 after ~30s
  -- merge [] xs  =  xs
  -- merge (x:xs) []  =  x:xs
  -- merge (x:xs) (y:ys)
  --   | x <= y  =  x:merge xs (y:ys)
  --   | otherwise  =  merge (y:x:xs) ys
  -- set target to 360 000 to reach it
  conjure "merge" merge'
    [ con ([] :: [Int])
    , fun ":" ((:) :: Int -> [Int] -> [Int])
    , fun "<=" ((<=) :: Int -> Int -> Bool)
    , guard
    , maxTests 1080
    , target 3600  -- set to 360 000 to reach solution
    ]

  -- unreachable: needs about 26, but can only reach 16
  conjure "merge" merge'
    [ con ([] :: [Int])
    , fun ":" ((:) :: Int -> [Int] -> [Int])
    , fun "compare" (compare :: Int -> Int -> Ordering)
    , ordcase (undefined :: [Int])
    , target 1080
    ]

  -- Produces a inefficient degenerate version of qsort
  -- where filter is applied _after_ sorting.
  conjure "qsort" sort'
    [ con ([] :: [Int])
    , fun ":" ((:) :: Int -> [Int] -> [Int])
    , fun "++" ((++) :: [Int] -> [Int] -> [Int])
    , fun "<=" ((<=) :: Int -> Int -> Bool)
    , fun ">"  ((>)  :: Int -> Int -> Bool)
    , fun "filter" (filter :: (Int -> Bool) -> [Int] -> [Int])
    -- if we disable the descent requirement and carry on,
    -- we eventually get the efficient qsort
    -- , dontRequireDescent
    -- , carryOn
    ]
