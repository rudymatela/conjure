-- ints.hs: conjuring functions over lists of ints
--
-- Copyright (C) 2021 Rudy Matela
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
import Conjure

second :: [Int] -> Int
second [x,y]  =  y
second [x,y,z]  =  y
second [x,y,z,w]  =  y

third :: [Int] -> Int
third [x,y,z]  =  z
third [x,y,z,w]  =  z

sum' :: [Int] -> Int
sum' [x]      =  x
sum' [x,y]    =  x+y
sum' [x,y,z]  =  x+y+z

product' :: [Int] -> Int
product' [x]      =  x
product' [x,y]    =  x*y
product' [x,y,z]  =  x*y*z

main :: IO ()
main = do
  conjure "second"  (second   :: [Int] -> Int) background
  conjure "third"   (third    :: [Int] -> Int) background
  conjure "sum"     (sum'     :: [Int] -> Int) background
  conjure "product" (product' :: [Int] -> Int) background

  conjureWithMaxSize 4 "sum"     (sum'     :: [Int] -> Int) backgroundWithFold
  conjureWithMaxSize 4 "product" (product' :: [Int] -> Int) backgroundWithFold

background :: [Expr]
background =
  [ val False
  , val True
  , val (0 :: Int)
  , val (1 :: Int)
  , value "+" ((+) :: Int -> Int -> Int)
  , value "*" ((*) :: Int -> Int -> Int)
  , value "null" (null :: [Int] -> Bool)
  , value "head" (head :: [Int] -> Int)
  , value "tail" (tail :: [Int] -> [Int])
  ]

backgroundWithFold :: [Expr]
backgroundWithFold  =
    value "foldr" (foldr :: (Int -> Int -> Int) -> Int -> [Int] -> Int)
  : background

-- sum xs      =  if null xs then 0 else head xs + sum (tail xs)
-- product xs  =  if null xs then 1 else head xs * product (tail xs)
--                1  2    3       4      5    6  7 8        9    10 symbols
