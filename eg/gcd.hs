-- gcd.hs: conjuring a GCD implementation
--
-- Copyright (C) 2021-2025 Rudy Matela
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
import Conjure

gcd' :: Int -> Int -> Int
gcd' 1 1  =  1
gcd' 1 2  =  1
gcd' 2 1  =  1
gcd' 2 2  =  2
gcd' 2 6  =  2
gcd' 6 2  =  2
gcd' 3 6  =  3
gcd' 6 3  =  3
gcd' 6 9  =  3
gcd' 9 6  =  3
gcd' 12 18  =  6

main :: IO ()
main = conjure "gcd a b" gcd'
  [ pr (0::Int)
  , prim "`mod`" (mod :: Int -> Int -> Int)
  ]
  -- desired function:
  -- gcd x 0  =  x
  -- gcd x y  =  gcd y (x `mod` y)
