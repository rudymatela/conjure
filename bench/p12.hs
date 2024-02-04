-- 12 background primitives
--
-- Copyright (C) 2021-2024 Rudy Matela
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
import Conjure
import System.Environment

conjureT :: Conjurable f => String -> f -> [Prim] -> IO ()
conjureT  =  conjureWith args{maxSize=1}

factorial :: Int -> Int
factorial 0  =  1
factorial 1  =  1
factorial 2  =  2
factorial 3  =  6
factorial 4  =  24
factorial 5  =  120

count' :: Int -> [Int] -> Int
count' 0 [0]  =  1
count' 0 [1]  =  0
count' 1 [0]  =  0
count' 1 [1]  =  1
count' 0 [0,0]  =  2
count' 0 [0,1]  =  1
count' 0 [1,2]  =  0
count' 1 [0,0]  =  0
count' 1 [0,1]  =  1
count' 1 [1,2]  =  1
count' 0 [0,0,0]  =  3
count' 0 [0,0,1]  =  2
count' 0 [1,0,0]  =  2

main :: IO ()
main  =  do
  putStrLn $ "running with " ++ show (length primitives) ++ " primitives"
  as <- getArgs
  case as of
    ["factorial"]     -> conjure  "factorial n" factorial primitives
    ["factorial","t"] -> conjureT "factorial n" factorial primitives
    ["sum"]           -> conjure  "sum"     (sum     :: [Int] -> Int) primitives
    ["sum","t"]       -> conjureT "sum"     (sum     :: [Int] -> Int) primitives
    ["product"]       -> conjure  "product" (product :: [Int] -> Int) primitives
    ["product","t"]   -> conjureT "product" (product :: [Int] -> Int) primitives
    ["length"]        -> conjure  "length"  (length  :: [Int] -> Int) primitives
    ["length","t"]    -> conjureT "length"  (length  :: [Int] -> Int) primitives
    ["count"]         -> conjure  "count"   count' $ primitives ++ primsCount
    ["count","t"]     -> conjureT "count"   count' $ primitives ++ primsCount
    _                 -> conjure  "factorial n" factorial primitives

primitives :: [Prim]
primitives  =
  [ pr (0::Int)
  , pr (1::Int)
  , prim "+" ((+) :: Int -> Int -> Int)
  , prim "*" ((*) :: Int -> Int -> Int)
  , prim "dec" (subtract 1 :: Int -> Int)

  , prim "==" ((==) :: Int -> Int -> Bool)

  , pr ([] :: [Int])
  , prim ":" ((:) :: Int -> [Int] -> [Int])
  , prim "head" (head :: [Int] -> Int)
  , prim "tail" (tail :: [Int] -> [Int])
  , prim "null" (null :: [Int] -> Bool)
  , prim "foldr" (foldr :: (Int -> Int -> Int) -> Int -> [Int] -> Int)
  , prim ".." (enumFromTo :: Int -> Int -> [Int])
  ]

primsCount :: [Prim]
primsCount  =
  [ prim "length" (length :: [Int] -> Int)
  , prim "filter" (filter :: (Int -> Bool) -> [Int] -> [Int])
  ]
