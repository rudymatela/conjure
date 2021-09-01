-- pow.hs: conjuring exponentiation
--
-- Copyright (C) 2021 Rudy Matela
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
import Conjure

pow :: Int -> Int -> Int
pow 2 0  =  1
pow 2 1  =  2
pow 2 2  =  4
pow 2 3  =  8
pow 3 2  =  9

main :: IO ()
main  =  do
  -- pow x 0  =  1
  -- pow x y  =  x * pow x (y - 1)
  conjureWithMaxSize 8 "pow" pow
    [ pr (0::Int)
    , pr (1::Int)
    , prim "*" ((*) :: Int -> Int -> Int)
    , prim "-" ((-) :: Int -> Int -> Int)
    ]

  -- pow b e  =  if e == 0 then 1 else pow b (halve e) * pow b (halve e) * if odd e then b else 1
  --             1  2  3 4      5      6   7  8     9 10 11 12  13   14 15 16 17  18    19     20
  -- -- OR --
  -- pow b 0  =  1
  -- pow b e  =  pow b (halve e) * pow b (halve e) * if odd e then b else 1
  --             2   3  4     5  6 7   8  9    10 11 12 13 14     15     16
  -- out of reach performance wise, OOM at size 9
  conjureWithMaxSize 6 "pow" pow
    [ pr (0::Int)
    , pr (1::Int)
--  , prim "sq" ((\x -> x*x) :: Int -> Int) -- cheat! OOM still
    , prim "*" ((*) :: Int -> Int -> Int)
    , prim "halve" ((`div` 2) :: Int -> Int)
    , prif (undefined :: Int)
    ]
