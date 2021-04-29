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
main  =  conjure "factorial" factorial background


background :: [Expr]
background  =
  [ val (0::Int)
  , val (1::Int)
  , value "+" ((+) :: Int -> Int -> Int)
  , value "*" ((*) :: Int -> Int -> Int)
  , value "dec" (subtract 1 :: Int -> Int)
  , value "isZero" ((==0) :: Int -> Bool)
  , val False
  , val True
  , ifFor (undefined :: Int)
  ]


-- the actual factorial function:
-- factorial x  =  if x == 0 then 1 else x * factorial (x - 1)
--                 1  2 3  4      5      6 7 8         9 10 11 symbols
--
-- OR
--
-- factorial x  =  if x == 0 then 1 else x * factorial (dec x)
--                 1  2 3  4      5      6 7 8          9  10 symbols
--
-- OR
--
-- factorial x  =  if (isZero x) then 1 else (x * factorial (dec x))
--                 1   2      3       4       5 6 7          8   9 symbols
