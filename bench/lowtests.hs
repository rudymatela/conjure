-- lowtests.hs: conjuring with a low number of tests
--
-- Copyright (C) 2021 Rudy Matela
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
--
-- With a low number of tests Conjure may not be able to find the actual
-- function due to Speculate finding incorrect properties from later properties
-- using reasoning.
{-# LANGUAGE CPP #-}
import Conjure
import Data.List (sort, transpose)

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

-- this function is one of the examples of MagicHaskeller
replicates' :: String -> Int -> String
replicates' [a]     1  =  [a]
replicates' [a,b]   1  =  [a,b]
replicates' [a]     2  =  [a,a]
replicates' [a,b]   2  =  [a,a,b,b]
replicates' [a,b,c] 2  =  [a,a,b,b,c,c]
replicates' [a]     3  =  [a,a,a]
replicates' [a,b]   3  =  [a,a,a,b,b,b]
replicates' [a,b,c] 3  =  [a,a,a,b,b,b,c,c,c]

as :: Args
as  =  args{showTheory = True}

main :: IO ()
main = do
  -- low number of tests, cannot conjure due to incorrect property
  conjureWith as{maxTests=60} "subset" (subset')
    [ prim "sort" (sort :: [Int] -> [Int])
    , prim "`isSubsequenceOf`" (isSubsequenceOf :: [Int] -> [Int] -> Bool)
    ]

  -- subset xs ys  =  sort xs `isSubsequenceOf` sort ys
  conjureWith as{maxTests=360} "subset" (subset')
    [ prim "sort" (sort :: [Int] -> [Int])
    , prim "`isSubsequenceOf`" (isSubsequenceOf :: [Int] -> [Int] -> Bool)
    ]

  -- low number of tests, cannot conjure due to incorrect property
  conjureWith as{maxTests=60} "replicates" replicates'
    [ prim "replicate" (replicate :: Int -> String -> [String])
    , prim "transpose" (transpose :: [[Char]] -> [[Char]])
    , prim "concat"    (concat :: [String] -> String)
    ]

  -- emulates how MagicHaskeller generates "replicates"
  conjureWith as{maxTests=360} "replicates" replicates'
    [ prim "replicate" (replicate :: Int -> String -> [String])
    , prim "transpose" (transpose :: [[Char]] -> [[Char]])
    , prim "concat"    (concat :: [String] -> String)
    ]
