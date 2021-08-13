-- gps2.hs: General Program Synthesis Benchmark Suite II
--
-- Copyright (C) 2021 Rudy Matela
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
{-# LANGUAGE CPP #-}
import Conjure
import System.Environment (getArgs)

import Data.List (findIndex, inits) -- for #1
import Data.Char (toUpper)          -- for #4


gps1p :: [Int] -> Maybe Int
gps1p [0,-1,2]   =  Just 1
gps1p [-1,0,1]   =  Just 0
gps1p [0,-1]     =  Just 1
gps1p [1,-1,-1]  =  Just 2

gps1g :: [Int] -> Maybe Int
gps1g xs  =  findIndex (0 >) (map sum (tail (inits xs)))

gps1p2 :: Int -> [Int] -> Int
gps1p2 0 [0,-1,2]   =  1
gps1p2 0 [-1,0,1]   =  0
gps1p2 0 [0,-1]     =  1
gps1p2 0 [1,-1,-1]  =  2
gps1p2 0 [0,0,0,-1]  =  3
gps1p2 0 [1,0,0,-2]  =  3

-- efficient gps1, unreachable performance-wise
gps1g2 :: Int -> [Int] -> Int
gps1g2 t []  =  undefined -- 1
gps1g2 t (x:xs)  =  if t + x < 0 -- 7
                    then 0 -- 8
                    else 1 + gps1g2 (t + x) xs -- 15

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

  conjureWithMaxSize 4 "gps1" gps1p2
    [ pr (0 :: Int)
    , pr (1 :: Int)
    , prim "+" ((+) :: Int -> Int -> Int)
    , prim "<" ((<) :: Int -> Int -> Bool)
    , prif (undefined :: Int)
    , prim "undefined" (undefined :: Int)
    ]


gps2p :: Double -> Double -> Int -> Double
gps2p 2 1 1  =  2 + 1
gps2p 2 1 2  =  2 + 1 + 1 + 0.5
gps2p 2 1 3  =  2 + 1 + 1 + 0.5 + 0.5 + 0.25
gps2p 3 1 1  =  3 + 1
gps2p 3 1 2  =  3 + 1 + 1/3

-- apex to apex
gps2g :: Double -> Double -> Int -> Double
gps2g h0 h1 0  =  h0 + h1
gps2g h0 h1 n  =  h0 + h1 + gps2g h1 (h1 * (h1 / h0)) (n - 1)
-- size 16 with dec, out of reach performance-wise

gps2c :: IO ()
gps2c  =  do
  conjureWithMaxSize 6 "gps2" gps2p
    [ pr (0 :: Int)
    , prim "dec" (subtract 1 :: Int -> Int)
    , prim "*" ((*) :: Double -> Double -> Double)
    , prim "/" ((/) :: Double -> Double -> Double)
    , prim "+" ((+) :: Double -> Double -> Double)
    ]


gps3p :: String -> Int
gps3p "X X X X X X X X X XXX"  =  300
gps3p "11 11 11 11 11 11 11 11 11 11"  =  20

-- unreachable performance wise
-- I presume a correct solution would need around 30 symbols
gps3c :: IO ()
gps3c  =  conjure "gps3" gps3p []



-- GPS #4: https://www.codewars.com/kata/517abf86da9663f1d2000003
-- Here we're using the problem description as in the GPS2 paper
-- considering we're getting input as kebab-case.
-- Nevertheless, this one is out of reach performance-wise (and OOM-wise)

gps4p :: String -> String
gps4p "the-stealth-warrior"  =  "theStealthWarrior"
--gps4p "The_Stealth_Warrior"  =  "TheStealthWarrior"
gps4p "camel-case"  =  "camelCase"
--gps4p "snake_case"  =  "snakeCase"
gps4p "Kebab-Case"  =  "KebabCase"

gps4g :: String -> String
gps4g ""  =  ""
-- gps4g ('-':c:cs)  =  toUpper c : gps4g cs
-- gps4g ('_':c:cs)  =  toUpper c : gps4g cs
gps4g (c:cs)  =  if c == '-'                                   --  5
                 then if null cs                               --  8
                      then ""                                  --  9
                      else toUpper (head cs) : gps4g (tail cs) -- 16
                 else c : gps4g cs                             -- 19

gps4c :: IO ()
gps4c  =  do
  let force  =  [ [val "the-stealth-warrior"]
--              , [val "The_Stealth_Warrior"]
                , [val "camel-case"]
--              , [val "snake_case"]
                , [val "Kebab-Case"]
                ]
  conjureWith args{forceTests = force, maxSize=6} "gps4" gps4p
    [ pr '-'
--  , pr '_'
    , pr ("" :: String)
    , prim ":" ((:) :: Char -> String -> String)
    , prim "==" ((==) :: Char -> Char -> Bool)
    , prim "head" (head :: String -> Char)
    , prim "tail" (tail :: String -> String)
    , prif (undefined :: Char)
    , prif (undefined :: String)
    , prim "toUpper" (toUpper :: Char -> Char)
    ]


main :: IO ()
main  =  do
  as <- getArgs
  case as of
    [] -> sequence_ gpss
    (n:_) -> gpss !! (read n - 1)


gpss :: [IO ()]
gpss  =  [ gps1c
         , gps2c
         , gps3c
         , gps4c
         ]
