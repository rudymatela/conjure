-- subset.hs: conjuring the subset function
--
-- Copyright (C) 2021 Rudy Matela
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
{-# LANGUAGE CPP #-}
import Conjure
import Data.List (sort)

#if __GLASGOW_HASKELL__ >= 710
import Data.List (isSubsequenceOf)
#else
isSubsequenceOf :: Eq a => [a] -> [a] -> Bool
isSubsequenceOf []    _  = True
isSubsequenceOf (_:_) [] = False
isSubsequenceOf (x:xs) (y:ys)
  | x == y    =    xs  `isSubsequenceOf` ys
  | otherwise = (x:xs) `isSubsequenceOf` ys
#endif

subset' :: [Int] -> [Int] -> Bool
subset' [] [x]  =  True
subset' [x] []  =  False
subset' [0] [0]  =  True
subset' [1] [1]  =  True
subset' [0] [1]  =  False
subset' [1] [0]  =  False
subset' [0] [0,1]  =  True
subset' [1] [0,1]  =  True
subset' [0] [1,0]  =  True
subset' [1] [1,0]  =  True
subset' [2] [0,1]  =  False
subset' [2] [1,0]  =  False
subset' [0,1] [0]  =  False
subset' [0,1] [1]  =  False
subset' [0,1] [0,1]  =  True
subset' [0,1] [1,0]  =  True
subset' [1,0] [0,1]  =  True
subset' [1,0] [1,0]  =  True
subset' [0,1,2] [0,1,2]  =  True
subset' [0,1,2,3] [0,1,2,3]  =  True

main :: IO ()
main = do
  -- subset xs ys  =  null xs || elem (head xs) ys && subset (tail xs) ys
  --                  1    2  3  4     5    6   7  8  9       10   11  12
  conjure "subset" (subset')
    [ val ([] :: [Int])
    , value "&&" (&&)
    , value "||" (||)
    , value "head" (head :: [Int] -> Int)
    , value "tail" (tail :: [Int] -> [Int])
    , value "null" (null :: [Int] -> Bool)
    , value "elem" (elem :: Int -> [Int] -> Bool)
    ]

  -- subset xs ys  =  sort xs `isSubsequenceOf` sort ys
  conjureWith args{maxTests=360} "subset" (subset')
    [ value "sort" (sort :: [Int] -> [Int])
    , value "`isSubsequenceOf`" (isSubsequenceOf :: [Int] -> [Int] -> Bool)
    ]
