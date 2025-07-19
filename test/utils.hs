-- Copyright (C) 2021-2025 Rudy Matela
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

  , prods [ [1], [2], [3] ] == [[1,2,3]]
  , prods [ [1,2], [3,4], [5] ] == [[1,3,5],[1,4,5],[2,3,5],[2,4,5]]
  , prods [ [1,2], [3], [4,5] ] == [[1,3,4],[1,3,5],[2,3,4],[2,3,5]]
  ]
