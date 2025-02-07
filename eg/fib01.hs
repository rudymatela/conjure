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
  -- These two examples show that currently
  -- functions with "many" argument (>=3)
  -- are particularly hard for conjure to synthesize.
  -- I've added an item in TODO.md to address this in 2025-02.

  -- It takes about 33 seconds to run with maxSize=8
  -- running with maxSize = 5 for faster runtime
  conjureWith args{maxSize=5, maxConstantSize=Just 1} "fib01" fib01
    [ pr (0::Int)
    , prim "dec" (subtract 1 :: Int -> Int)
    , prim "+" ((+) :: Int -> Int -> Int)
    ]

  -- It takes about 27 seconds to run with maxSize=12
  -- running with maxSize = 9 for faster runtime
  conjureWith args{usePatterns = False, maxSize = 1, maxConstantSize=Just 1} "fib01" fib01
    [ pr (0::Int)
    , prim "+" ((+) :: Int -> Int -> Int)
    , prim "dec" (subtract 1 :: Int -> Int)
    , prim "<=" ((<=) :: Int -> Int -> Bool)
    , prif (undefined :: Int)
    ]
  -- expected function:
  -- fib01 x y z  =  if z <= 0                     -- 4
  --                 then y                        -- 5
  --                 else fib01 y (x + y) (dec z)  -- 12
