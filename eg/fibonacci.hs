-- fibonacci.hs: conjuring a fibonacci function
--
-- Copyright (C) 2021-2025 Rudy Matela
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
import Conjure

fibonacci :: Int -> Int
fibonacci 0  =  1
fibonacci 1  =  1
fibonacci 2  =  2
fibonacci 3  =  3
fibonacci 4  =  5
fibonacci 5  =  8
fibonacci 6  =  13

main :: IO ()
main  =  do
  conjure "fibonacci n" fibonacci
    [ pr (0::Int)
    , pr (1::Int)
    , pr (2::Int)
    , prim "+" ((+) :: Int -> Int -> Int)
    , prim "-" ((-) :: Int -> Int -> Int)
    ]
-- expected function:
-- fibonacci 0  =  1
-- fibonacci 1  =  1
-- fibonacci x  =  fibonacci (x - 1) + fibonacci (x - 2)
