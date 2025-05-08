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
    [ con (0 :: Int)
    , con (1 :: Int)
    , fun "halve" ((`div` 2) :: Int -> Int)
    , fun "parity" ((`mod` 2) :: Int -> Int)
    , fun "+" ((+) :: Int -> Int -> Int)
    ]
  -- bitsum 0  =  0
  -- bitsum n  =  parity n + bitsum (halve n)

  conjure "bitsum" bitsum
    [ con (0 :: Int)
    , con (1 :: Int)
    , con (2 :: Int)
    , fun "+" ((+) :: Int -> Int -> Int)
    , fun "`div`" (div :: Int -> Int -> Int)
    , fun "`mod`" (mod :: Int -> Int -> Int)
    ]
  -- bitsum 0  =  0
  -- bitsum n  =  n `mod` 2 + bitsum (n `div` 2)
