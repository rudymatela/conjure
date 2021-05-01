-- list.hs: conjuring functions over lists (of ints)
--
-- Copyright (C) 2021 Rudy Matela
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
import Conjure
import Data.List (insert)

length' :: [Int] -> Int
length' []       =  0
length' [x]      =  1
length' [x,y]    =  2
length' [x,y,z]  =  3

reverse' :: [Int] -> [Int]
reverse' []       =  []
reverse' [x]      =  [x]
reverse' [x,y]    =  [y,x]
reverse' [x,y,z]  =  [z,y,x]

sort' :: [Int] -> [Int]
sort' []       =  []
sort' [x]      =  [x]
sort' [x,y]
  | x <= y     =  [x,y]
  | otherwise  =  [y,x]
sort' [x,y,z]
  | x <= y && y <= z  =  [x,y,z]
  | z <= y && y <= x  =  [z,y,x]

main :: IO ()
main = do
  -- length xs  =  if null xs then 0 else 1 + length (tail xs)
  --               1  2    3       4      5 6 7       8    9
  conjure "length" length'
    [ val (0 :: Int)
    , val (1 :: Int)
    , value "+" ((+) :: Int -> Int -> Int)
    , value "tail" (tail :: [Int] -> [Int])
    , value "null" (null :: [Int] -> Bool)
    ]

  -- reverse xs  =  if null xs then [] else reverse (tail xs) ++ [head xs]
  --                1  2    3       4       5        6    7   8  9 10 11 12
  -- needs size 11 with unit
  conjureWithMaxSize 9 "reverse" reverse'
    [ val ([] :: [Int])
    , value "unit" ((:[]) :: Int -> [Int])
    , value "++" ((++) :: [Int] -> [Int] -> [Int])
    , value "head" (head :: [Int] -> Int)
    , value "tail" (tail :: [Int] -> [Int])
    , value "null" (null :: [Int] -> Bool)
    ]

  -- sort xs  =  if null xs then [] else insert (head xs) (sort (tail xs))
  --             1  2    3       4       5       6    7    8     9    10
  conjureWithMaxSize 10 "sort" sort'
    [ val ([] :: [Int])
    , value "insert" (insert :: Int -> [Int] -> [Int])
    , value "head" (head :: [Int] -> Int)
    , value "tail" (tail :: [Int] -> [Int])
    , value "null" (null :: [Int] -> Bool)
    ]
