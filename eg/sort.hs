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
    [ pr ([] :: [Int])
    , prim "insert" (insert :: Int -> [Int] -> [Int])
    , prim "head" (head :: [Int] -> Int)
    , prim "tail" (tail :: [Int] -> [Int])
    , prim "null" (null :: [Int] -> Bool)
    ]

  -- now through fold
  -- sort xs  =  foldr insert [] xs
  conjure "sort" sort'
    [ pr ([] :: [Int])
    , prim "insert" (insert :: Int -> [Int] -> [Int])
    , prim "foldr" (foldr :: (Int -> [Int] -> [Int]) -> [Int] -> [Int] -> [Int])
    ]

  -- an insert function
  conjureWith args{target=50400} "insert" insert'
    [ prim "[]" ([] :: [Int])
    , prim ":" ((:) :: Int -> [Int] -> [Int])
    , prim "<=" ((<=) :: Int -> Int -> Bool)
    , guard
    ]

  -- qsort []  =  []                           -- 1
  -- qsort (x:xs)  =  qsort (filter (x >) xs)  -- 6
  --            ++ (x:qsort (filter (x <=) xs) -- 14
  -- this one is not out of reach performance wise,
  -- but is not generated because of the deconstruction restriction.
  -- The following does generate a correct but inneficient version of qsort.
  conjure "qsort" sort'
    [ pr ([] :: [Int])
    , prim ":" ((:) :: Int -> [Int] -> [Int])
    , prim "++" ((++) :: [Int] -> [Int] -> [Int])
    , prim "<=" ((<=) :: Int -> Int -> Bool)
    , prim ">"  ((>)  :: Int -> Int -> Bool)
    , prim "filter" (filter :: (Int -> Bool) -> [Int] -> [Int])
    ]

  -- if we disable the descent requirement, we get the efficient qsort
  -- though with a larger search space
  conjureWith args{requireDescent=False} "qsort" sort'
    [ pr ([] :: [Int])
    , prim ":" ((:) :: Int -> [Int] -> [Int])
    , prim "++" ((++) :: [Int] -> [Int] -> [Int])
    , prim "<=" ((<=) :: Int -> Int -> Bool)
    , prim ">"  ((>)  :: Int -> Int -> Bool)
    , prim "filter" (filter :: (Int -> Bool) -> [Int] -> [Int])
    ]

  -- found!  candidate #1703311 @ size 22
  -- merge [] xs  =  xs
  -- merge (x:xs) []  =  x:xs
  -- merge (x:xs) (y:ys)
  --   | x <= y  =  x:merge xs (y:ys)
  --   | otherwise  =  merge (y:x:xs) ys
  -- set target to 2 000 000 to reach it
  conjureWith args{target=10080, maxTests=1080} "merge" merge'
    [ pr ([] :: [Int])
    , prim ":" ((:) :: Int -> [Int] -> [Int])
    , prim "<=" ((<=) :: Int -> Int -> Bool)
    , guard
    ]

  -- unreachable: needs about 26, but can only reach 16
  conjureWith args{target=1080} "merge" merge'
    [ prim ":" ((:) :: Int -> [Int] -> [Int])
    , prim "compare" (compare :: Int -> Int -> Ordering)
    , primOrdCaseFor (undefined :: [Int])
    ]
