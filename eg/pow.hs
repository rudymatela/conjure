-- pow.hs: conjuring exponentiation
--
-- Copyright (C) 2021-2025 Rudy Matela
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
  conjure "pow" pow
    [ con (0::Int)
    , con (1::Int)
    , fun "*" ((*) :: Int -> Int -> Int)
    , fun "-" ((-) :: Int -> Int -> Int)
    ]

  -- pow b 0  =  1
  -- pow b e
  --   | odd e  =  b * pow b (e - 1)             -- 11
  --   | otherwise  =  square (pow b (halve e))  -- 16
  conjure "pow" pow
    [ con (0::Int)
    , con (1::Int)
    , fun "square" ((\x -> x*x) :: Int -> Int) -- cheat
    , fun "*" ((*) :: Int -> Int -> Int)
    , fun "-" ((-) :: Int -> Int -> Int)
    , fun "halve" ((`div` 2) :: Int -> Int) -- cheat
    , fun "odd" (odd :: Int -> Bool)
    , guard
    , maxSize 6 -- remove to find the first version...
    -- simply out of reach:
    -- , maxSize 16, carryOn
    ]
