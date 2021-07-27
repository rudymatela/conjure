-- sort.hs: conjuring a sort function
--
-- Copyright (C) 2021 Rudy Matela
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
import Conjure

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

main :: IO ()
main = do
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
