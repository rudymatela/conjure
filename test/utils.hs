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

  , isDeconstructor n ([]::[Int]) tail
  , isDeconstructor n (0::Int) (\x -> x-1)
  ]

isDeconstructor :: (Eq a, Ord a, Listable a, Show a) => Int -> a -> (a -> a) -> Bool
isDeconstructor m z f  =  holds m $
  \x -> x > z ==> length (take m $ takeWhile (> z) (iterate f x)) < m
-- TODO: takeWhileNext (/=) in addition to takeWhile
