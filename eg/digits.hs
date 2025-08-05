-- digits.hs: functions over digits
--
-- Copyright (C) 2025 Rudy Matela
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
import Conjure

-- decimal digit sum
digitSum :: Int -> Int
digitSum 0  =  0
digitSum 12  =  3
digitSum 42  =  6
digitSum 1337  =  14
digitSum 31337  =  17

digitCount :: Int -> Int -> Int
digitCount 2 42  =  2
digitCount 1 111  =  3
digitCount 4 144  =  144
digitCount 0 1080  =  2
digitCount 3 1337  =  2
digitCount 3 31337  =  3

digits :: Int -> [Int]
digits 12  =  [1,2]
digits 42  =  [4,2]
digits 1337  =  [1,3,3,7]
digits 31337  =  [3,1,3,3,7]

main :: IO ()
main  =  do
  conjure "revdigits" (reverse . digits)
    [ con ([] :: [Int])
    , fun ":" ((:) :: Int -> [Int] -> [Int])
    , fun "`div`" (div :: Int -> Int -> Int)
    , fun "`mod`" (mod :: Int -> Int -> Int)
    , con (0 :: Int)
    , con (10 :: Int)
    ]

  conjure "digits" digits
    [ con ([] :: [Int])
    , fun ":" ((:) :: Int -> [Int] -> [Int])
    , fun "++" ((++) :: [Int] -> [Int] -> [Int])
    , fun "`div`" (div :: Int -> Int -> Int)
    , fun "`mod`" (mod :: Int -> Int -> Int)
    , con (0 :: Int)
    , con (10 :: Int)
    ]

  conjure "digitSum" digitSum
    [ con (0 :: Int)
    , con (10 :: Int)
    , fun "+" ((+) :: Int -> Int -> Int)
    , fun "`div`" (div :: Int -> Int -> Int)
    , fun "`mod`" (mod :: Int -> Int -> Int)
    ]

  -- out-of-reach performance-wise
  conjure "digitCount" digitCount
    [ con (0 :: Int)
    , con (10 :: Int)
    , fun "+" ((+) :: Int -> Int -> Int)
    , fun "`div`" (div :: Int -> Int -> Int)
    , fun "`mod`" (mod :: Int -> Int -> Int)
    , fun "==" ((==) :: Int -> Int -> Bool)
    , guard
    , maxTests 1080
    ]
