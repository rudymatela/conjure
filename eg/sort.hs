-- sort.hs: conjuring a sort function
--
-- Copyright (C) 2021-2025 Rudy Matela
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
import Conjure
import Data.List (insert, sort)

sort' :: [Int] -> [Int]
sort' []       =  []
sort' [x]      =  [x]
sort' [x,y]
  | x <= y     =  [x,y]
  | otherwise  =  [y,x]
sort' [x,y,z]
  | x <= y && y <= z  =  [x,y,z]
  | x <= z && z <= y  =  [x,z,y]
  | y <= x && x <= z  =  [y,x,z]
  | y <= z && z <= x  =  [y,z,x]
  | z <= x && x <= y  =  [z,x,y]
  | z <= y && y <= x  =  [z,y,x]

insert' :: Int -> [Int] -> [Int]
insert' 0 []  =  [0]
insert' 0 [1,2]  =  [0,1,2]
insert' 1 [0,2]  =  [0,1,2]
insert' 2 [0,1]  =  [0,1,2]

-- merge' :: [Int] -> [Int] -> [Int]
-- merge' xs ys  =  sort (xs ++ ys)

merge' :: [Int] -> [Int] -> [Int]
merge' [] []  =  []
merge' xs []  =  xs
merge' [] ys  =  ys
merge' [x] [y] | x <= y     =  [x,y]
               | otherwise  =  [y,x]
merge' [0,1] [0,1]  =  [0,0,1,1]
merge' [0,1] [2,3]  =  [0,1,2,3]
merge' [0,2] [1,3]  =  [0,1,2,3]
merge' [0,1] [1,2]  =  [0,1,1,2]
merge' [1,2] [0,1]  =  [0,1,1,2]
merge' [0,2] [1,1]  =  [0,1,1,2]

main :: IO ()
main = do
  -- recursive insertion sort
  -- sort []  =  []
  -- sort (x:xs)  =  insert x (sort xs)
  conjure "sort" sort'
    [ con ([] :: [Int])
    , fun "insert" (insert :: Int -> [Int] -> [Int])
    , fun "head" (head :: [Int] -> Int)
    , fun "tail" (tail :: [Int] -> [Int])
    , fun "null" (null :: [Int] -> Bool)
    ]

  -- now through fold
  -- sort xs  =  foldr insert [] xs
  conjure "sort" sort'
    [ con ([] :: [Int])
    , fun "insert" (insert :: Int -> [Int] -> [Int])
    , fun "foldr" (foldr :: (Int -> [Int] -> [Int]) -> [Int] -> [Int] -> [Int])
    ]

  -- an insert function
  conjure "insert" insert'
    [ fun "[]" ([] :: [Int])
    , fun ":" ((:) :: Int -> [Int] -> [Int])
    , fun "<=" ((<=) :: Int -> Int -> Bool)
    , guard
    , target 50400
    ]

  -- qsort []  =  []                           -- 1
  -- qsort (x:xs)  =  qsort (filter (x >) xs)  -- 6
  --            ++ (x:qsort (filter (x <=) xs) -- 14
  -- this one is not out of reach performance wise,
  -- but is not generated because of the deconstruction restriction.
  -- The following does generate a correct but inneficient version of qsort.
  conjure "qsort" sort'
    [ con ([] :: [Int])
    , fun ":" ((:) :: Int -> [Int] -> [Int])
    , fun "++" ((++) :: [Int] -> [Int] -> [Int])
    , fun "<=" ((<=) :: Int -> Int -> Bool)
    , fun ">"  ((>)  :: Int -> Int -> Bool)
    , fun "filter" (filter :: (Int -> Bool) -> [Int] -> [Int])
    ]

  -- if we disable the descent requirement and carry on,
  -- we eventually get the efficient qsort
  -- with a larger search space
  {-
  conjure "qsort" sort'
    [ con ([] :: [Int])
    , fun ":" ((:) :: Int -> [Int] -> [Int])
    , fun "++" ((++) :: [Int] -> [Int] -> [Int])
    , fun "<=" ((<=) :: Int -> Int -> Bool)
    , fun ">"  ((>)  :: Int -> Int -> Bool)
    , fun "filter" (filter :: (Int -> Bool) -> [Int] -> [Int])
    , dontRequireDescent
    , carryOn
    ]
  -}

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
    [ fun ":" ((:) :: Int -> [Int] -> [Int])
    , fun "compare" (compare :: Int -> Int -> Ordering)
    , ordcase (undefined :: [Int])
    , target 1080
    ]
