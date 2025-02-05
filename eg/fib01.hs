-- fib01.hs: conjuring an efficient fibonacci function
--
-- Copyright (C) 2021-2025 Rudy Matela
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
import Conjure

fib01 :: Int -> Int -> Int -> Int
-- fib01 x y 0  =  x  -- even with this, out of reach
fib01 0 1 0  =  1
fib01 0 1 1  =  1
fib01 0 1 2  =  2
fib01 0 1 3  =  3
fib01 0 1 4  =  5
fib01 0 1 5  =  8
fib01 0 1 6  =  13
fib01 0 1 7  =  21

fibonacci :: Int -> Int
fibonacci  =  f 0 1
  where
  f x y 0  =  y
  f x y n  =  f y (x + y) (n - 1)

main :: IO ()
main  =  do
  conjureWithMaxSize 5 "fib01" fib01
    [ pr (0::Int)
    , prim "dec" (subtract 1 :: Int -> Int)
    , prim "+" ((+) :: Int -> Int -> Int)
    ]

  -- takes about 22 seconds to run with maxSize=12
  conjureWith args{usePatterns = False, maxSize = 10} "fib01" fib01
    [ pr (0::Int)
    , prim "+" ((+) :: Int -> Int -> Int)
    , prim "dec" (subtract 1 :: Int -> Int)
    , prim "<=" ((<=) :: Int -> Int -> Bool)
    ]
-- expected function:
-- fib01 x y z  =  if z <= 0 then y else fib01 y (x + y) (dec z)
--                 1  2 3  4      5      6     7  8 9 10  11 12

  -- out of reach as well:
  -- conjure "fib01" fib01
  --   [ pr (0::Int)
  --   , pr (1::Int)
  --   , prim "+" ((+) :: Int -> Int -> Int)
  --   , prim "-" ((-) :: Int -> Int -> Int)
  --   ]
