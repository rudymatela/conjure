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
main  =  conjureWithMaxSize 10 "factorial n" factorial background


background :: [Expr]
background  =
  [ val (0::Int)
  , val (1::Int)
  , value "+" ((+) :: Int -> Int -> Int)
  , value "*" ((*) :: Int -> Int -> Int)
  , value "dec" (subtract 1 :: Int -> Int)
  , value "==" ((==) :: Int -> Int -> Bool)
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
