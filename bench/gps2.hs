-- gps2.hs: General Program Synthesis Benchmark Suite II
--
-- Copyright (C) 2021 Rudy Matela
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
{-# LANGUAGE CPP #-}
import Conjure
import System.Environment (getArgs)

import Data.List (findIndex, inits)               -- for  #1
import Data.Char (toUpper)                        -- for  #4
import Data.Ratio ((%))                           -- for  #7
import Data.List (findIndices, tails, isPrefixOf) -- for #12


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


-- GPS #5: Coin Sums
-- https://projecteuler.net/problem=31
--
-- The problem description is inconsistent with the one given in the paper.
--
-- Paper: change-making problem in USD
-- Euler: How many different ways can £2 be made using any number of coins?
--
-- I'm considering the Paper to be canonical, as the one in Project Euler can be solved by a constant function:
--
-- solution :: Int
-- solution  =  6378216738  -- <-- arbitrary example here, I didn't run the numbers
gps5p :: Int -> [Int]
gps5p 100  =  [4, 0, 0, 0]
gps5p  50  =  [2, 0, 0, 0]
gps5p  25  =  [1, 0, 0, 0]
gps5p  30  =  [1, 0, 1, 0]
gps5p  20  =  [0, 2, 0, 0]
gps5p   3  =  [0, 0, 0, 3]

gps5p2 :: [Int] -> Int -> [Int]
gps5p2 [25, 10, 5, 1] 100  =  [4, 0, 0, 0]
gps5p2 [25, 10, 5, 1]  50  =  [2, 0, 0, 0]
gps5p2 [25, 10, 5, 1]  25  =  [1, 0, 0, 0]
gps5p2 [25, 10, 5, 1]  30  =  [1, 0, 1, 0]
gps5p2 [25, 10, 5, 1]  20  =  [0, 2, 0, 0]
gps5p2 [25, 10, 5, 1]   3  =  [0, 0, 0, 3]

coins :: [Int]
coins  =  [25, 10, 5, 1]

gps5g :: Int -> [Int]
gps5g  =  tell coins

tell :: [Int] -> Int -> [Int]
tell []     a  =  []
tell (n:ns) a  =  a `div` n : tell ns (a `mod` n)

gps5c :: IO ()
gps5c  =  do
  -- cannot conjure directly due to needing to introduce a local definition
  conjure "gps5" gps5p
    [
    ]

  let force = [ [val coins, val (100::Int)]
              , [val coins, val ( 50::Int)]
              , [val coins, val ( 25::Int)]
              , [val coins, val ( 30::Int)]
              , [val coins, val ( 20::Int)]
              , [val coins, val (  3::Int)]
              ]
  conjureWith args{forceTests=force} "tell" gps5p2
    [ pr ([] :: [Int])
    , prim ":" ((:) :: Int -> [Int] -> [Int])
    , prim "`div`" (div :: Int -> Int -> Int)
    , prim "`mod`" (mod :: Int -> Int -> Int)
    ]

  conjureWith args{forceTests=force} "gps5" gps5p
    [ pr coins
    , prim "tell" tell
    ]

  conjureWith args{forceTests=force} "gps5" gps5p
    [ pr (1 :: Int)
    , pr (5 :: Int)
    , pr (10 :: Int)
    , pr (25 :: Int)
    , pr ([] :: [Int])
    , prim ":" ((:) :: Int -> [Int] -> [Int])
    , prim "tell" tell
    ]


-- GPS2#6: Cut Vector
gps6g :: [Int] -> Int
gps6g xs  =  snd
          $  minimum
          [  (abs (sum xs0 - sum xs1), i)
          |  i <- [0 .. length xs]
          ,  let (xs0, xs1) = splitAt i xs
          ]

-- not Conjurable due to needing local definition
-- I conjecture it _perhaps-maybe_ this could be done in 3 steps,
-- but I'll leave this as-is for now.
gps6c :: IO ()
gps6c  =
  conjure "gps6" gps6g []


-- GPS2#7: Dice Game
-- https://projecteuler.net/problem=31
-- https://github.com/thelmuth/Clojush/blob/psb2-v1.0/src/clojush/problems/psb2/dice_game.clj
-- again, paper and problem are inconsistent, going with paper
gps7p :: Integer -> Integer -> Rational
gps7p 4 6  =  1 % 4
gps7p 6 4  =  7 % 12
gps7p 2 4  =  1 % 8
gps7p 4 2  =  5 % 8
gps7p 2 6  =  1 % 12
gps7p 6 2  =  3 % 4

gps7g :: Integer -> Integer -> Rational
gps7g peter colin  =  sum (map (min colin) [0 .. (peter-1)]) % (colin*peter)

-- out of reach performance-wise
gps7c :: IO ()
gps7c  =  do
  conjureWith args{maxSize=6} "gps7" gps7p $ take 0
    [ pr (0 :: Integer)
    , pr (1 :: Integer)
    , prim "%" ((%) :: Integer -> Integer -> Rational)
    , prim "+" ((+) :: Integer -> Integer -> Integer)
    , prim "-" ((-) :: Integer -> Integer -> Integer)
    , prim "*" ((*) :: Integer -> Integer -> Integer)
    , prim "min" (min :: Integer -> Integer -> Integer)
    , prim ".." (enumFromTo :: Integer -> Integer -> [Integer])
    , prim "map" (map :: (Integer -> Integer) -> [Integer] -> [Integer])
    , prim "sum" (sum :: [Integer] -> Integer)
    ]


gps8p :: Int -> [Int] -> (Int,Int)
gps8p 2 [1,1,2]  =  (1,1)
gps8p 3 [1,1,2]  =  (1,2)
gps8p 2 [0,1,0,2]  =  (0,2)

gps8g :: Int -> [Int] -> (Int,Int)
gps8g x xs  =  head [(y,z) | y <- xs, z <- xs, y + z == x]
-- this one could be generated but is a bit of a stretch...
-- it is unintuitive to provide the given symbols
-- gps8g :: Int -> [Int] -> (Int,Int)
-- gps8g x xs  =  head $ filter ((x ==) . uncurry (+)) $ liftA2 (,) xs xs

gps8c :: IO ()
gps8c  =  conjure "gps" gps8p []


-- GPSB#9: Fizz Buzz (CW)
gps9p :: Int -> String
gps9p 3  =  "Fizz"
gps9p 4  =  "4"
gps9p 5  =  "Buzz"
gps9p 6  =  "Fizz"
gps9p 10  =  "Buzz"
gps9p 15  =  "FizzBuzz"
gps9p 17  =  "17"

gps9g :: Int -> String
gps9g x
  | x `div` 3 == 0  =  "Fizz" -- 7
  | x `div` 5 == 0  =  "Buzz" -- 14
  | x `div` 3 == 0 && x `div` 5 == 0  =  "FizzBuzz" -- 27
  | otherwise       =  show x -- 29

-- probably unreachable performance-wise
gps9c :: IO ()
gps9c  =  conjure "gps" gps9p []


gps10p :: [Int] -> Int
gps10p [1,2,3]  =  -5
gps10p [10,20,30]  =  13

-- does not do rounding
gps10g :: [Int] -> Int
gps10g xs  =  sum $ map (\x -> x `div` 3 - 2) xs

-- unreachable due to lambda
gps10c :: IO ()
gps10c  =  conjure "gps" gps9p []


gps11p :: Int -> Int -> Int
gps11p  =  gcd'
  where
  gcd' :: Int -> Int -> Int
  gcd' 1 1  =  1
  gcd' 1 2  =  1
  gcd' 2 1  =  1
  gcd' 2 2  =  2
  gcd' 2 6  =  2
  gcd' 6 2  =  2
  gcd' 3 6  =  3
  gcd' 6 3  =  3
  gcd' 6 9  =  3
  gcd' 9 6  =  3
  gcd' 12 18  =  6

gps11c :: IO ()
gps11c  =  conjureWith args{requireDescent=False} "gcd a b" gps11p
  [ pr (0::Int)
  , prim "`mod`" (mod :: Int -> Int -> Int)
  ]
  -- generated function:
  -- gcd x 0  =  x
  -- gcd x y  =  gcd y (x `mod` y)


gps12p :: String -> String -> [Int]
gps12p "a"   "a"   =  [0]
gps12p "aa"  "a"   =  [0,1]
gps12p "aa"  "aa"  =  [0]
gps12p "a a" "a"   =  [0,2]
gps12p "b"   "a"   =  []

gps12g :: String -> String -> [Int]
gps12g s s'  =  findIndices (s' `isPrefixOf`) (tails s)

gps12c :: IO ()
gps12c  =  conjure "gps12" gps12p
  [ prim "findIndices" (findIndices :: (String -> Bool) -> [String] -> [Int])
  , prim "`isPrefixOf`" (isPrefixOf :: String -> String -> Bool)
  , prim "tails" (tails :: String -> [String])
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
         , gps5c
         , gps6c
         , gps7c
         , gps8c
         , gps9c
         , gps10c
         , gps11c
         , gps12c
         ]
