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
sum' [x]      =  x
sum' [x,y]    =  x+y
sum' [x,y,z]  =  x+y+z

product' :: [Int] -> Int
product' [x]      =  x
product' [x,y]    =  x*y
product' [x,y,z]  =  x*y*z

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
