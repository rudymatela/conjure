-- gcd.hs: conjuring a GCD implementation
--
-- Copyright (C) 2021 Rudy Matela
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
import Conjure

gcd' :: Int -> Int -> Int
gcd' 1 1  =  1
gcd' 1 2  =  1
gcd' 2 2  =  2
gcd' 2 6  =  2
gcd' 3 6  =  3
gcd' 9 6  =  3
gcd' 12 18  =  6

main :: IO ()
main = conjure "gcd a b" gcd'
  [ val (0::Int)
  , value "`mod`" (mod :: Int -> Int -> Int)
  , value "==" ((==) :: Int -> Int -> Bool)
  ]
  -- desired function:
  -- gcd a b  =  if b == 0 then a else gcd b (a `mod` b)
  --             1  2 3  4      5      6   7  8  9   10
  -- TODO: make it so that conjure is able to find this
