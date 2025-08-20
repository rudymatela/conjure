-- fib01.hs: conjuring an efficient fibonacci function
--
-- Copyright (C) 2021-2025 Rudy Matela
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
import Conjure


-- using a single type , we hang after size 7, we need 9
-- so we split the types here to nudge Conjure in the right direction
fib01' :: Integer -> Integer -> Int -> Integer
fib01' 0 1 1  =  1
fib01' 0 1 2  =  1
fib01' 0 1 3  =  2
fib01' 0 1 4  =  3
fib01' 0 1 5  =  5
fib01' 0 1 6  =  8
fib01' 0 1 7  =  13

fibonacci' :: Int -> Int
fibonacci' 0  =  0
fibonacci' 1  =  1
fibonacci' 2  =  1
fibonacci' 3  =  2
fibonacci' 4  =  3
fibonacci' 5  =  5
fibonacci' 6  =  8


main :: IO ()
main  =  do
  conjure "fib01" fib01'
    [ con (0::Int)
    , con (1::Int)
    , fun "+" ((+) :: Int -> Int -> Int) -- uneeded
    , fun "-" ((-) :: Int -> Int -> Int)
    , fun "+" ((+) :: Integer -> Integer -> Integer)
    ]

  conjure "fibonacci" fibonacci'
    [ con (0::Int)
    , con (1::Int)
    , fun "fib01" fib01
    ]


-- our goals:

fibonacci :: Int -> Int
fibonacci =  f 0 1
  where
  f x y 0  =  x
  f x y n  =  f y (x + y) (n - 1)

fib01 :: Int -> Int -> Int -> Int
fib01 x y n | n < 0  =  error "undefined for negatives"
fib01 x y 0  =  x
fib01 x y n  =  fib01 y (x + y) (n - 1)
