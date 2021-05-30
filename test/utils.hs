-- Copyright (C) 2021 Rudy Matela
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).

import Test

main :: IO ()
main  =  mainTest tests 5040

tests :: Int -> [Bool]
tests n  =
  [ True

  , holds n $ \x -> iterateUntil (==) (`quot` (2 :: Int)) x == 0
  , holds n $ \xs -> iterateUntil (==) (drop 1) xs == ([]::[Bool])

  , holds n $ \xs ys -> length xs == length ys
                    ==> zipWith (<>) xs ys == mzip xs (ys :: [[Int]])

  , holds n $ \xs ys -> length xs >= length ys
                    ==> zipWith (<>) xs (ys <> repeat mempty) == mzip xs (ys :: [[Int]])

  , takeUntil (== 5) [1..] == [1,2,3,4]
  , takeUntil (> 4)  [1..] == [1,2,3,4]

  , takeNextWhile (<) []  ==  ([]::[Int])
  , takeNextWhile (<) [0]  ==  [0]
  , takeNextWhile (<) [0,1]  ==  [0,1]
  , takeNextWhile (<) [0,1,2]  ==  [0,1,2]
  , takeNextWhile (<) [0,1,2,1,0]  ==  [0,1,2]
  , takeNextWhile (/=) [3,2,1,0,0,0] == [3,2,1,0]
  , takeNextUntil (==) [3,2,1,0,0,0] == [3,2,1,0]
  , takeNextUntil (>) [0,1,2,1,0] == [0,1,2]

  , deconstructions null tail [1,2,3 :: Int]
    == [ [1,2,3]
       , [2,3]
       , [3]
       ]
  , deconstructions null tail ([] :: [Int]) == []
  , deconstructions (==0) (`div`2) 15
    == [15, 7, 3, 1]

  ,       isDeconstruction m (null :: [A] -> Bool) tail
  , not $ isDeconstruction m (null :: [A] -> Bool) id
  ,       isDeconstruction m (null :: [A] -> Bool) (drop 1)
  ,       isDeconstruction m (null :: [A] -> Bool) (drop 2)
  ,       isDeconstruction m (null :: [A] -> Bool) (drop 3)
  ,       isDeconstruction m (<0) (\x -> x-1 :: Int)
  ,       isDeconstruction m (<0) (\x -> x-2 :: Int)
  ,       isDeconstruction m (==0) (\x -> x-1 :: Int)
  , not $ isDeconstruction m (==0) (\x -> x-2 :: Int)
  ,       isDeconstruction m (==0) (\x -> x `div` 2 :: Int)
  ,       isDeconstruction m (==0) (\x -> x `quot` 2 :: Int)
  ]
  where
  m  =  n `div` 60


-- Checks if the given pair of functions are a valid deconstruction
-- for Listable values.
-- The deconstruction is considered valid if it converges
-- for more than 50% of values.
isDeconstruction :: Listable a
                 => Int
                 -> (a -> Bool) -> (a -> a) -> Bool
isDeconstruction m z d  =  count is (take m list) >= (m `div` 2)
  where
  is x  =  length (take m $ deconstructions z d x) < m
