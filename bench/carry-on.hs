-- carryon.hs: conjuring implementations of a factorial function
--
-- Copyright (C) 2021-2024 Rudy Matela
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
import Conjure

factorial :: Int -> Int
factorial 1  =  1
factorial 2  =  2
factorial 3  =  6
factorial 4  =  24


main :: IO ()
main  =  do
  -- carry on Conjuring larger implementations
  conjureWith args{carryOn = True, maxSize = 11} "factorial n" factorial
    [ pr (0::Int)
    , pr (1::Int)
    , prim "+" ((+) :: Int -> Int -> Int)
    , prim "*" ((*) :: Int -> Int -> Int)
    , prim "-" ((-) :: Int -> Int -> Int)
    ]
  -- should produce:
  -- factorial 0  =  1
  -- factorial x  =  x * factorial (x - 1)
  --
  -- and
  -- factorial 0  =  1
  -- factorial 1  =  1
  -- factorial x  =  x * factorial (x - 1)
  --
  -- among other implementations
