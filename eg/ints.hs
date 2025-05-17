-- ints.hs: conjuring functions over lists of ints
--
-- Copyright (C) 2021-2025 Rudy Matela
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
sum' [1]      =  1
sum' [1,2]    =  3
sum' [1,2,3]  =  6

product' :: [Int] -> Int
product' [2,3]    =  6
product' [3,3]    =  9
product' [2,3,4]  =  24

main :: IO ()
main = do
  conjure "second"  (second :: [Int] -> Int)
    [ fun "null" (null :: [Int] -> Bool)
    , fun "head" (head :: [Int] -> Int)
    , fun "tail" (tail :: [Int] -> [Int])
    ]

  conjure "third"   (third :: [Int] -> Int)
    [ fun "null" (null :: [Int] -> Bool)
    , fun "head" (head :: [Int] -> Int)
    , fun "tail" (tail :: [Int] -> [Int])
    ]

  conjure "sum"     (sum' :: [Int] -> Int)
    [ con (0 :: Int)
    , con (1 :: Int)
    , fun "+" ((+) :: Int -> Int -> Int)
    , fun "*" ((*) :: Int -> Int -> Int)
    ]

  conjure "product" (product' :: [Int] -> Int)
    [ con (0 :: Int)
    , con (1 :: Int)
    , fun "+" ((+) :: Int -> Int -> Int)
    , fun "*" ((*) :: Int -> Int -> Int)
    ]

  conjure "sum"     (sum' :: [Int] -> Int) ingredientsWithFold
  conjure "product" (product' :: [Int] -> Int) ingredientsWithFold

ingredients :: [Ingredient]
ingredients =
  [ con (0 :: Int)
  , con (1 :: Int)
  , fun "+" ((+) :: Int -> Int -> Int)
  , fun "*" ((*) :: Int -> Int -> Int)
  ]

ingredientsWithFold :: [Ingredient]
ingredientsWithFold  =
    fun "foldr" (foldr :: (Int -> Int -> Int) -> Int -> [Int] -> Int)
  : ingredients
