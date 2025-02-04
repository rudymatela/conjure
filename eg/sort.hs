-- sort.hs: conjuring a sort function
--
-- Copyright (C) 2021-2024 Rudy Matela
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

main :: IO ()
main = do
  -- recursive insertion sort
  -- sort xs  =  if null xs then [] else insert (head xs) (sort (tail xs))
  --             1  2    3       4       5       6    7    8     9    10
  -- -- OR --
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
  conjureWithMaxSize 18 "insert" insert'
    [ prim "[]" ([] :: [Int])
    , prim ":" ((:) :: Int -> [Int] -> [Int])
    , prim "<=" ((<=) :: Int -> Int -> Bool)
    , prif (undefined :: [Int])
    ]

  -- qsort
  -- qsort xs  =  if null xs                                 --  3
  --              then []                                    --  4
  --              else qsort (filter (< head xs) (tail xs))  -- 11
  --                ++ (head xs:[])                          -- 16
  --                ++ qsort (filter (>= head xs) (tail xs)) -- 24
  -- not only this is out of reach performance wise,
  -- but the needed recursive calls will not be enumerated
  -- -- OR --
  -- qsort []  =  []                           -- 1
  -- qsort (x:xs)  =  qsort (filter (x >) xs)  -- 6
  --            ++ (x:qsort (filter (x <=) xs) -- 14
  -- this one is not out of reach performance wise,
  -- but is not generated because of the deconstruction restriction.
  -- The following does generate a correct but inneficient version of qsort.
  conjureWith args{maxSize=14} "qsort" sort'
    [ pr ([] :: [Int])
    , prim ":" ((:) :: Int -> [Int] -> [Int])
    , prim "++" ((++) :: [Int] -> [Int] -> [Int])
    , prim "<=" ((<=) :: Int -> Int -> Bool)
    , prim ">"  ((>)  :: Int -> Int -> Bool)
    , prim "filter" (filter :: (Int -> Bool) -> [Int] -> [Int])
    ]

  -- merge [] []  =  []
  -- merge (x:xs) (y:ys)  =  if x <= y then x:merge xs (y:ys) else y:merge (x:xs) ys
  --                         2  3 4  5      678     9 10 11 12  13 14 15  16 17 18 19
  -- OOM after size 17, out of reach performance wise
  -- update: cannot reach at size 19 on lapmatrud OOM
  conjureWith args{maxSize=12} "merge" merge'
    [ pr ([] :: [Int])
    , prim ":" ((:) :: Int -> [Int] -> [Int])
    , prim "<=" ((<=) :: Int -> Int -> Bool)
    , prif (undefined :: [Int])
    ]

  -- unreachable: needs about 26, but can only reach 16
  conjureWithMaxSize 12 "merge" merge'
    [ prim ":" ((:) :: Int -> [Int] -> [Int])
    , prim "compare" (compare :: Int -> Int -> Ordering)
    , primOrdCaseFor (undefined :: [Int])
    ]
