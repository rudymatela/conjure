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
  conjure "second"  (second   :: [Int] -> Int) primitives
  conjure "third"   (third    :: [Int] -> Int) primitives
  conjure "sum"     (sum'     :: [Int] -> Int) primitives
  conjure "product" (product' :: [Int] -> Int) primitives

  conjure "sum"     (sum'     :: [Int] -> Int) primitivesWithFold
  conjure "product" (product' :: [Int] -> Int) primitivesWithFold

primitives :: [Prim]
primitives =
  [ pr (0 :: Int)
  , pr (1 :: Int)
  , prim "+" ((+) :: Int -> Int -> Int)
  , prim "*" ((*) :: Int -> Int -> Int)
  , prim "null" (null :: [Int] -> Bool)
  , prim "head" (head :: [Int] -> Int)
  , prim "tail" (tail :: [Int] -> [Int])
  ]

primitivesWithFold :: [Prim]
primitivesWithFold  =
    prim "foldr" (foldr :: (Int -> Int -> Int) -> Int -> [Int] -> Int)
  : primitives

-- sum xs      =  if null xs then 0 else head xs + sum (tail xs)
-- product xs  =  if null xs then 1 else head xs * product (tail xs)
--                1  2    3       4      5    6  7 8        9    10 symbols
