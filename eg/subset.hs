-- subset.hs: conjuring the subset function
--
-- Copyright (C) 2021-2025 Rudy Matela
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
  -- subset [] ys  =  True
  -- subset (x:xs) ys  =  elem x ys && subset xs ys
  conjure "subset" (subset')
    [ pr ([] :: [Int])
    , pr True
    , pr False
    , prim "&&" (&&)
    , prim "||" (||)
    , prim "elem" (elem :: Int -> [Int] -> Bool)
    ]

  -- subset xs ys  =  sort xs `isSubsequenceOf` sort ys
  conjure "subset" (subset')
    [ prim "sort" (sort :: [Int] -> [Int])
    , prim "`isSubsequenceOf`" (isSubsequenceOf :: [Int] -> [Int] -> Bool)
    ]
