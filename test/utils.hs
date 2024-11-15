-- Copyright (C) 2021-2024 Rudy Matela
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).

import Test

main :: IO ()
main  =  mainTest tests 5040

tests :: Int -> [Bool]
tests n  =
  [ True

  , holds n $ \xs ys -> length xs == length ys
                    ==> zipWith (<>) xs ys == mzip xs (ys :: [[Int]])

  , holds n $ \xs ys -> length xs >= length ys
                    ==> zipWith (<>) xs (ys <> repeat mempty) == mzip xs (ys :: [[Int]])

  , sets []  ==  [[] :: [Int]]
  , sets [1]  ==  [[1], [] :: [Int]]
  , sets [1,2]  ==  [[1,2], [1], [2], [] :: [Int]]
  , sets [1,2,3]  ==  [[1,2,3],[1,2],[1,3],[1],[2,3],[2],[3],[] :: [Int]]
  ]
