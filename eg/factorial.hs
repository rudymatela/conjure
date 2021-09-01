-- factorial.hs: conjuring a factorial function
--
-- Copyright (C) 2021 Rudy Matela
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
import Conjure

factorial :: Int -> Int
factorial 1  =  1
factorial 2  =  2
factorial 3  =  6
factorial 4  =  24


main :: IO ()
main  =  do
  -- explicit recursion
  conjure "factorial n" factorial
    [ pr (0::Int)
    , pr (1::Int)
    , prim "+" ((+) :: Int -> Int -> Int)
    , prim "*" ((*) :: Int -> Int -> Int)
    , prim "-" ((-) :: Int -> Int -> Int)
    ]
  -- should produce:
  -- factorial 0  =  1
  -- factorial x  =  x * factorial (x - 1)

  -- using foldr and enumFromTo
  conjure "factorial n" factorial
    [ pr (0::Int)
    , pr (1::Int)
    , prim "+" ((+) :: Int -> Int -> Int)
    , prim "*" ((*) :: Int -> Int -> Int)
    , prim "-" ((-) :: Int -> Int -> Int)
    , prim ".." (enumFromTo :: Int -> Int -> [Int])
    , prim "foldr" (foldr :: (Int -> Int -> Int) -> Int -> [Int] -> Int)
    ]
  -- should produce:
  -- factorial x  =  foldr (*) 1 [1..x]
