-- carryon.hs: conjuring implementations of a factorial function
--
-- Copyright (C) 2021-2025 Rudy Matela
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
  conjure "factorial n" factorial
    [ unfun (0::Int)
    , unfun (1::Int)
    , fun "+" ((+) :: Int -> Int -> Int)
    , fun "*" ((*) :: Int -> Int -> Int)
    , fun "-" ((-) :: Int -> Int -> Int)
    , carryOn
    , maxSize 11
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
