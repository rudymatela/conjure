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

  -- Found!  It takes about 12 seconds to run with maxSize=8
  -- running with maxSize = 5 for faster runtime
  conjure "fib01" fib01
    [ unfun (0::Int)
    , fun "dec" (subtract 1 :: Int -> Int)
    , fun "+" ((+) :: Int -> Int -> Int)
    , maxSize 5
    , maxConstantSize 1
    ]

  -- It takes about 27 seconds to run with maxSize=12
  -- running with maxSize = 9 for faster runtime
  conjure "fib01" fib01
    [ unfun (0::Int)
    , fun "+" ((+) :: Int -> Int -> Int)
    , fun "dec" (subtract 1 :: Int -> Int)
    , fun "<=" ((<=) :: Int -> Int -> Bool)
    , iif (undefined :: Int)

    , singlePattern
    , maxSize 1
    , maxConstantSize 1
    ]
  -- expected function:
  -- fib01 x y z  =  if z <= 0                     -- 4
  --                 then y                        -- 5
  --                 else fib01 y (x + y) (dec z)  -- 12
