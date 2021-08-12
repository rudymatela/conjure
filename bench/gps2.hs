-- gps2.hs: General Program Synthesis Benchmark Suite II
--
-- Copyright (C) 2021 Rudy Matela
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
{-# LANGUAGE CPP #-}
import Conjure
import System.Environment (getArgs)

import Data.List (findIndex, inits) -- for #1


gps1p :: [Int] -> Maybe Int
gps1p [0,-1,2]   =  Just 1
gps1p [-1,0,1]   =  Just 0
gps1p [0,-1]     =  Just 1
gps1p [1,-1,-1]  =  Just 2

gps1g :: [Int] -> Maybe Int
gps1g xs  =  findIndex (0 >) (map sum (tail (inits xs)))

gps1c :: IO ()
gps1c  =  do
  conjure "gps1" gps1p
    [ pr (0 :: Int)
    , prim "+" ((+) :: Int -> Int -> Int)
    , prim ">" ((>) :: Int -> Int -> Bool)
    , prim "foldr" (foldr :: (Int -> Int -> Int) -> Int -> [Int] -> Int)
    , prim "sum" (sum :: [Int] -> Int)
    , prim "findIndex" (findIndex :: (Int -> Bool) -> [Int] -> Maybe Int)
    , prim "map" (map :: ([Int] -> Int) -> [[Int]] -> [Int])
    , prim "inits" (inits :: [Int] -> [[Int]])
    , prim "tail" (tail :: [[Int]] -> [[Int]])
    ]

  conjure "gps1" gps1p
    [ pr (0 :: Int)
    , prim "+" ((+) :: Int -> Int -> Int)
    , prim ">" ((>) :: Int -> Int -> Bool)
    , prim "sum" (sum :: [Int] -> Int)
    , prim "foldr" (foldr :: (Int -> Int -> Int) -> Int -> [Int] -> Int)
    , prim "findIndex" (findIndex :: (Int -> Bool) -> [Int] -> Maybe Int)
    , prim "map" (map :: ([Int] -> Int) -> [[Int]] -> [Int])
    , prim "inits" (inits :: [Int] -> [[Int]])
    , prim "tail" (tail :: [[Int]] -> [[Int]])
    ]


main :: IO ()
main  =  do
  as <- getArgs
  case as of
    [] -> sequence_ gpss
    (n:_) -> gpss !! (read n - 1)


gpss :: [IO ()]
gpss  =  [ gps1c
         ]
