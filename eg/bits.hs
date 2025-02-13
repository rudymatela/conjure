-- bits.hs: functions over bits
--
-- Copyright (C) 2025 Rudy Matela
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
import Conjure

-- binary digit sum
-- binaryDigitSum
bitsum :: Int -> Int
bitsum 0  =  0  --   0
bitsum 1  =  1  --   1
bitsum 2  =  1  --  10
bitsum 3  =  2  --  11
bitsum 4  =  1  -- 100
bitsum 5  =  2  -- 101
bitsum 6  =  2  -- 110
bitsum 7  =  3  -- 111

main :: IO ()
main  =  do
  conjure "bitsum" bitsum
    [ pr (0 :: Int)
    , pr (1 :: Int)
    , prim "halve" ((`div` 2) :: Int -> Int)
    , prim "parity" ((`mod` 2) :: Int -> Int)
    , prim "+" ((+) :: Int -> Int -> Int)
    ]
  -- bitsum 0  =  0
  -- bitsum n  =  parity n + bitsum (halve n)

  conjure "bitsum" bitsum
    [ pr (0 :: Int)
    , pr (1 :: Int)
    , pr (2 :: Int)
    , prim "+" ((+) :: Int -> Int -> Int)
    , prim "`div`" (div :: Int -> Int -> Int)
    , prim "`mod`" (mod :: Int -> Int -> Int)
    ]
  -- bitsum 0  =  0
  -- bitsum n  =  n `mod` 2 + bitsum (n `div` 2)
