-- factorial.hs: conjuring a factorial function
--
-- Copyright (C) 2021 Rudy Matela
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
import Conjure

factorial :: Int -> Int
factorial 0  =  1
factorial 1  =  1
factorial 2  =  2
factorial 3  =  6
factorial 4  =  24
factorial 5  =  120


main :: IO ()
main  =  do
  -- using enumFromTo
  conjure "factorial n" factorial
    [ pr (1::Int)
    , prim ".." (enumFromTo :: Int -> Int -> [Int])
    , prim "*" ((*) :: Int -> Int -> Int)
    , prim "foldr" (foldr :: (Int -> Int -> Int) -> Int -> [Int] -> Int)
    ]

  -- explicit recursion
  conjure "factorial n" factorial
    [ pr (0::Int)
    , pr (1::Int)
    , prim "+" ((+) :: Int -> Int -> Int)
    , prim "*" ((*) :: Int -> Int -> Int)
    , prim "dec" (subtract 1 :: Int -> Int)
    ]

-- the actual factorial function:
-- factorial n  =  if n == 0 then 1 else n * factorial (n - 1)
--                 1  2 3  4      5      6 7 8         9 10 11 symbols
--
-- OR
--
-- factorial n  =  if n == 0 then 1 else n * factorial (dec n)
--                 1  2 3  4      5      6 7 8          9  10 symbols
--
-- OR
--
-- factorial n  =  if (isZero n) then 1 else (n * factorial (dec n))
--                 1   2      3       4       5 6 7          8   9 symbols


{-
-- Paramorphism of Naturals encoded as integers
para :: (Int -> b -> b) -> b -> Int -> b
para (?) z  =  p
  where
  p n  |  n < 0  =  z  -- no negatives for you :-)
  p 0  =  z
  p n  =  n ? p (n-1)

The following works with a maxSize of 4, but not with a maxSize of 5.

  -- using a paramorphism
  conjure "factorial n" factorial
    [ pr (1::Int)
    , prim "para" (para :: (Int->Int->Int) -> Int -> Int -> Int)
    , prim "*" ((*) :: Int -> Int -> Int)
    ]


the factorial function is the following:

fact  =  para (*) 1


now consider the following grow_fast function:

grow_fast  =  para (para (*)) 1  :: Integer -> Integer


> growFast 1
1
> growFast 2
2
> growFast 3
6
> growFast 4
2880
> growFast 5
7148302174930174893017438921... 8000 digits!
> growFast 6
stack overflow
-}
