-- longshot.hs: miscellaneous longshots
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
  | z <= y && y <= x  =  [z,y,x]

pow :: Int -> Int -> Int
pow 2 0  =  1
pow 2 1  =  2
pow 2 2  =  4
pow 2 3  =  8
pow 3 2  =  9

main :: IO ()
main = do
  -- qsort
  -- qsort xs  =  if null xs                                 -- 3
  --              then []                                    -- 4
  --              else qsort (filter (< head xs) (tail xs))  -- 11
  --                ++ (head xs:[])                          -- 16
  --                ++ qsort (filter (>= head xs) (tail xs)) -- 24
  -- not only this is out of reach performance wise,
  -- but the needed recursive calls will not be enumerated
  conjure "qsort" sort'
    [ val ([] :: [Int])
    , value ":" ((:) :: Int -> [Int] -> [Int])
    , value "head" (head :: [Int] -> Int)
    , value "tail" (tail :: [Int] -> [Int])
    , value "null" (null :: [Int] -> Bool)
    , value "++" ((++) :: [Int] -> [Int] -> [Int])
    , value "<" ((<) :: Int -> Int -> Bool)
    , value ">=" ((>=) :: Int -> Int -> Bool)
    , value "filter" (filter :: (Int -> Bool) -> [Int] -> [Int])
    ]

  -- pow b e  =  if e == 0 then 1 else b * pow b (dec e)
  --             1  2  3 4      5      6 7 8   9  10 11
  -- somehow this takes 30s to run, the two arguments
  -- of the same type introduce the difficulty here.
  conjureWithMaxSize 9 "pow" pow
    [ val (0::Int)
    , val (1::Int)
    , value "+" ((+) :: Int -> Int -> Int)
    , value "*" ((*) :: Int -> Int -> Int)
    , value "dec" (subtract 1 :: Int -> Int)
    , value "==" ((==) :: Int -> Int -> Bool)
    ]
