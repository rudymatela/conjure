-- gps2.hs: General Program Synthesis Benchmark Suite II
--
-- Copyright (C) 2021-2025 Rudy Matela
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
{-# LANGUAGE CPP, TemplateHaskell #-}
import Conjure
import System.Environment (getArgs)

import Data.List (findIndex, inits)               -- for  #1
import Data.Char (toUpper)                        -- for  #4
import Data.Ratio ((%))                           -- for  #7
import Data.List (findIndices, tails, isPrefixOf) -- for #12
import Data.Maybe (fromJust)                      -- for #23
import Test.LeanCheck                             -- for #24
import Data.Express                               -- for #24


-- PSB2 #1 -- Basement (AoC) ---
-- Given a vector of integers, return the first index such that the sum of all
-- integers from the start of the vector to the index (inclusive) is negative

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
    [ unfun (0 :: Int)
    , fun "+" ((+) :: Int -> Int -> Int)
    , fun ">" ((>) :: Int -> Int -> Bool)
    , fun "foldr" (foldr :: (Int -> Int -> Int) -> Int -> [Int] -> Int)
    , fun "sum" (sum :: [Int] -> Int)
    , fun "findIndex" (findIndex :: (Int -> Bool) -> [Int] -> Maybe Int)
    , fun "map" (map :: ([Int] -> Int) -> [[Int]] -> [Int])
    , fun "inits" (inits :: [Int] -> [[Int]])
    , fun "tail" (tail :: [[Int]] -> [[Int]])
    ]

  conjure "gps1" gps1p
    [ unfun (0 :: Int)
    , fun "+" ((+) :: Int -> Int -> Int)
    , fun ">" ((>) :: Int -> Int -> Bool)
    , fun "sum" (sum :: [Int] -> Int)
    , fun "foldr" (foldr :: (Int -> Int -> Int) -> Int -> [Int] -> Int)
    , fun "findIndex" (findIndex :: (Int -> Bool) -> [Int] -> Maybe Int)
    , fun "map" (map :: ([Int] -> Int) -> [[Int]] -> [Int])
    , fun "inits" (inits :: [Int] -> [[Int]])
    , fun "tail" (tail :: [[Int]] -> [[Int]])
    ]

  conjure "gps1" gps1p2
    [ unfun (0 :: Int)
    , unfun (1 :: Int)
    , fun "+" ((+) :: Int -> Int -> Int)
    , fun "<" ((<) :: Int -> Int -> Bool)
    , guard
    , fun "undefined" (undefined :: Int)
    , maxSize 4
    , maxEquationSize 4
    ]


-- PSB2 #2 -- Bouncing Balls (CW) --

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
-- size 17, out of reach performance-wise

gps2c :: IO ()
gps2c  =  do
  conjure "gps2" gps2p
    [ unfun (0 :: Int)
    , unfun (1 :: Int)
    , fun "*" ((*) :: Double -> Double -> Double)
    , fun "/" ((/) :: Double -> Double -> Double)
    , fun "+" ((+) :: Double -> Double -> Double)
    , fun "-" ((-) :: Double -> Double -> Double)
    , maxSize 6
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

gps4s :: (String -> String) -> [Property]
gps4s g  =
  [ property $ g "the-stealth-warrior" == "theStealthWarrior"
  , property $ g "camel-case" == "camelCase"
  , property $ g "Kebab-Case" == "KebabCase"
  ]

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
  conjureFromSpec "gps4" gps4s
    [ unfun '-'
    , unfun ("" :: String)
    , fun ":" ((:) :: Char -> String -> String)
    , fun "==" ((==) :: Char -> Char -> Bool)
    , fun "head" (head :: String -> Char)
    , fun "tail" (tail :: String -> String)
    , iif (undefined :: Char)
    , iif (undefined :: String)
    , fun "toUpper" (toUpper :: Char -> Char)
    , maxSize 6
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

gps5s :: ([Int] -> Int -> [Int]) -> [Property]
gps5s t  =
  [ property $ t [25, 10, 5, 1] 100 == [4, 0, 0, 0]
  , property $ t [25, 10, 5, 1]  50 == [2, 0, 0, 0]
  , property $ t [25, 10, 5, 1]  25 == [1, 0, 0, 0]
  , property $ t [25, 10, 5, 1]  30 == [1, 0, 1, 0]
  , property $ t [25, 10, 5, 1]  20 == [0, 2, 0, 0]
  , property $ t [25, 10, 5, 1]   3 == [0, 0, 0, 3]
  ]

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

  conjureFromSpec "tell" gps5s
    [ unfun ([] :: [Int])
    , fun ":" ((:) :: Int -> [Int] -> [Int])
    , fun "`div`" (div :: Int -> Int -> Int)
    , fun "`mod`" (mod :: Int -> Int -> Int)
    ]

  conjure "gps5" gps5p
    [ unfun coins
    , fun "tell" tell
    ]

  conjure "gps5" gps5p
    [ unfun (1 :: Int)
    , unfun (5 :: Int)
    , unfun (10 :: Int)
    , unfun (25 :: Int)
    , unfun ([] :: [Int])
    , fun ":" ((:) :: Int -> [Int] -> [Int])
    , fun "tell" tell
    , target 50400
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
  conjure "gps7" gps7p $ take 0
    [ unfun (0 :: Integer)
    , unfun (1 :: Integer)
    , fun "%" ((%) :: Integer -> Integer -> Rational)
    , fun "+" ((+) :: Integer -> Integer -> Integer)
    , fun "-" ((-) :: Integer -> Integer -> Integer)
    , fun "*" ((*) :: Integer -> Integer -> Integer)
    , fun "min" (min :: Integer -> Integer -> Integer)
    , fun ".." (enumFromTo :: Integer -> Integer -> [Integer])
    , fun "map" (map :: (Integer -> Integer) -> [Integer] -> [Integer])
    , fun "sum" (sum :: [Integer] -> Integer)
    , maxSize 6
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
gps8c  =  conjure "gps8" gps8p []


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
  | x `mod` 3 == 0  =  "Fizz" -- 7
  | x `mod` 5 == 0  =  "Buzz" -- 14
  | x `mod` 3 == 0 && x `mod` 5 == 0  =  "FizzBuzz" -- 27
  | otherwise       =  show x -- 29

-- unreachable performance-wise:
-- too many candidates to sift through
-- even in two steps
gps9c :: IO ()
gps9c  =  conjure "gps9" gps9p
  [ unfun "Fizz"
  , unfun "Buzz"
  , unfun "FizzBuzz"
  , unfun (0 :: Int)
  , unfun (3 :: Int)
  , unfun (5 :: Int)
  , unfun False
  , unfun True
  , fun "`divBy`" divBy
--, fun "`mod`" (mod :: Int -> Int -> Int)
  , fun "&&" (&&)
--, fun "==" ((==) :: Int -> Int -> Bool)
  , fun "show" (show :: Int -> String)
  , iif (undefined :: String)
  , target 1080 -- so this fails quickly...
  ]
  where
  -- two-step try:
  divBy :: Int -> Int -> Bool
  x `divBy` 0  =  False
  x `divBy` y  =  x `mod` y == 0


gps10p :: [Int] -> Int
gps10p [0]  =  -2
gps10p [3]  =  -1
gps10p [6]  =  0
gps10p [9]  =  1
gps10p [0,3]  =  -3
gps10p [3,0]  =  -3
gps10p [3,2,1]  =  -5
gps10p [1,2,3]  =  -5
gps10p [10,20,30]  =  13

-- does not do rounding
gps10g :: [Int] -> Int
gps10g []  =  0
gps10g (x:xs)  =  (x `div` 3 - 2) + gps10g xs

-- unreachable due to lambda
gps10c :: IO ()
gps10c  =  conjure "gps10" gps10p
  [ unfun (0 :: Int)
  , unfun (1 :: Int)
  , unfun (2 :: Int)
  , unfun (3 :: Int)
  , fun "`div`" (div :: Int -> Int -> Int)
  , fun "+" ((+) :: Int -> Int -> Int)
  , fun "-" ((-) :: Int -> Int -> Int)
  , target 50400
  ]


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
gps11c  =  conjure "gcd a b" gps11p
  [ unfun (0::Int)
  , fun "`mod`" (mod :: Int -> Int -> Int)
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
  [ fun "findIndices" (findIndices :: (String -> Bool) -> [String] -> [Int])
  , fun "`isPrefixOf`" (isPrefixOf :: String -> String -> Bool)
  , fun "tails" (tails :: String -> [String])
  ]


gps13p :: [Int] -> [Int]
gps13p [0,1]  =  [1]
gps13p [1,0]  =  [1,0]
gps13p [2,0,1]  =  [2,1]
gps13p [1,0,1]  =  [1]

gps13g :: [Int] -> [Int]
gps13g  =  leaders
  where
  leaders []  =  []
  leaders (x:xs)  =  if all (x >) xs
                     then x : leaders xs
                     else leaders xs

gps13c :: IO ()
gps13c  =  conjure "gps13_leaders" gps13p
  [ unfun ([] :: [Int])
  , fun ":" ((:) :: Int -> [Int] -> [Int])
  , fun ">" ((>) :: Int -> Int -> Bool)
  , fun "<" ((<) :: Int -> Int -> Bool)
  , fun "all" (all :: (Int -> Bool) -> [Int] -> Bool)
  , fun "any" (any :: (Int -> Bool) -> [Int] -> Bool)
  , guard
  ]


gps14p :: [Int] -> Int
gps14p  =  undefined

-- 30 symbols
gps14g :: [Int] -> Int
gps14g  =  luhn
  where
  luhn :: [Int] -> Int
  luhn xs  =  sum (firsts xs ++ map double9 (seconds xs))
    where
    double9 :: Int -> Int
    double9 x  =  if xx > 9
                  then xx - 9
                  else xx
      where
      xx  =  x * 2
    seconds :: [Int] -> [Int]
    seconds []  =  []
    seconds (x:y:xs)  =  y : seconds xs
    firsts :: [Int] -> [Int]
    firsts []  =  []
    firsts (x:y:xs)  =  x : seconds xs

-- cannot Conjure directly
gps14c :: IO ()
gps14c  =  conjure "gps14_luhn" gps14p
  [
  ]


gps15p :: () -> ()
gps15p  =  undefined

-- skipped
gps15c :: IO ()
gps15c  =  conjure "gps15_mastermind" gps15p []


gps16p :: String -> String
gps16p "a"      =  "a"
gps16p "aaa"    =  "a"
gps16p "aa"     =  "aa"
gps16p "a a"    =  " "
gps16p " a "    =  "a"
gps16p "a a "   =  " a"
gps16p " a a "  =  " "
gps16p "aba"    =  "b"
gps16p "bab"    =  "a"
gps16p "abba"   =  "bb"
gps16p "aaaaa"  =  "a"

gps16g1 :: String -> String
gps16g1 s  =  if odd len
              then take 1 (drop (len `div` 2) s)
              else take 2 (drop (len `div` 2 - 1) s)
  where
  len  =  length s

gps16g2 :: String -> String
gps16g2 ""  =  ""
gps16g2 (c:cs)  =  if length cs <= 1
                   then c : cs
                   else gps16g2 (init cs)

gps16c :: IO ()
gps16c  =  conjure "gps16_middle" gps16p
  [ unfun ""
  , unfun (1 :: Int)
  , fun "<=" ((<=) :: Int -> Int -> Bool)
  , fun ":" ((:) :: Char -> String -> String)
  , fun "length" (length :: String -> Int)
  , fun "init" (init :: String -> String)
  , iif (undefined :: String) -- TODO: can't replace by guard, why? (July 2025)
  -- update: it is enumerated, but somehow does not passes the tests, why?
  ]


-- PSB2 #17 -- Paired Digits (AoC) --
-- Given a string of digits,
-- return the sum of the digits
-- whose following digit are the same.
-- Originally called "Inverse Captcha":
-- https://adventofcode.com/2017/day/1

gps17p :: [Int] -> Int
gps17p [0,1,0]  =  0
gps17p [1,1]  =  1
gps17p [0,0,1,1]  =  1
gps17p [1,1,0]  =  1
gps17p [0,1,1]  =  1
gps17p [1,1,1]  =  2
gps17p [0,1,2]  =  0
gps17p [1,1,2,2]  =  3
gps17p [1,1,1,1]  =  3  -- variation from advent
gps17p [1,2,3,4]  =  0

gps17g :: [Int] -> Int
gps17g xs  =  pds xs
  where
  pds []  =  0
  pds (x:xs)  =  if not (null xs) && x == head xs
                 then x + pds xs
                 else pds xs
-- surprise:
-- gps17_pds []  =  0
-- gps17_pds (x:xs)  =  (if not (null xs) && head xs == x then x else 0) + gps17_pds xs


-- can generate at size 15 in 6
-- setting limit of 5 for faster automated tests
-- BENCHMARK: increase maxSize from 6 to 18
gps17c :: IO ()
gps17c  =  do
  conjure "gps17_pds" gps17p
    [ unfun (0 :: Int)
    , guard
    , fun "not" not
    , fun "null" (null :: [Int] -> Bool)
    , fun "==" ((==) :: Int -> Int -> Bool)
    , fun "head" (head :: [Int] -> Int)
    , fun "+" ((+) :: Int -> Int -> Int)
    , fun "&&" (&&)
    , maxSize 6 -- 18
    ]

  -- OOM after size 16, unreachable by increasing pattern depth
  conjure "gps17_pds" gps17p
    [ unfun (0 :: Int)
    , guard
    , fun "==" ((==) :: Int -> Int -> Bool)
    , fun "+" ((+) :: Int -> Int -> Int)
    , maxPatternDepth 2
    , maxSize 6 -- 18
    ]


gps18p :: [Double] -> [Double] -> Double
gps18p [1.0] [0.5]  =  0.5
gps18p [2.0] [0.5]  =  1.0
gps18p [1.0,1.0] [0.5,0.0]  =  1.5
gps18p [1.0,1.0] [0.0,0.5]  =  1.5

gps18g :: [Double] -> [Double] -> Double
gps18g prices discounts  =  foldr (+) 0 (zipWith (*) prices (map (1-) discounts))

-- this was OOM'd
gps18c :: IO ()
gps18c  =  conjure "gps18_price" gps18p
  [ unfun (0 :: Double)
  , unfun (1 :: Double)
  , fun "+" ((+) :: Double -> Double -> Double)
  , fun "*" ((*) :: Double -> Double -> Double)
  , fun "-" ((-) :: Double -> Double -> Double)
--  , fun "foldr" (foldr :: (Double -> Double -> Double) -> Double -> [Double] -> Double)
--   , fun "zipWith" (zipWith :: (Double -> Double -> Double) -> [Double] -> [Double] -> [Double])
--  , fun "map" (map :: (Double -> Double) -> [Double] -> [Double])
  , maxSize 6
  ]


gps19p :: Int -> Double -> Double -> Double -> Double
gps19p 0 0 1.0 1.0  =  0
gps19p 1 0 1.0 1.0  =  0
gps19p 2 0 1.0 1.0  =  0
gps19p 1 0 1.0 2.0  =  1.0
gps19p 2 0 1.0 2.0  =  2.0
gps19p 1 0 0.5 1.0  =  0.5
gps19p 2 0 0.5 1.0  =  1.0

gps19g :: Int -> Double -> Double -> Double -> Double
gps19g 0 total melt fall  =  total
gps19g n total melt fall  =  gps19p (n-1) (max 0 (total-melt+fall)) melt fall
-- size 14, out of reach performance wise.

gps19c :: IO ()
gps19c  =  conjure "gps19_snowday" gps19p
  [ unfun (0 :: Int)
  , unfun (1 :: Int)
  , fun "max" (max :: Double -> Double -> Double)
  , fun "+" ((+) :: Double -> Double -> Double)
  , fun "-" ((-) :: Double -> Double -> Double)
  , maxSize 6
  ]


gps20p :: String -> Bool
gps20p  =  undefined

gps20c :: IO ()
gps20c  =  conjure "gps20" gps20p
  [
  ]


gps21s :: (String -> String) -> [Property]
gps21s s  =
  [ property $ s "word" == "word"
  , property $ s "words" == "sdrow"
  , property $ s "word words" == "word sdrow"
  , property $ s "words word" == "sdrow word"
  ]

spinSpec :: (String -> String) -> [Property]
spinSpec spin  =
  [ property $ spin "abc" == "abc"
  , property $ spin "abcd" == "abcd"
  , property $ spin "word" == "word"
  , property $ spin "abcde" == "edcba"
  , property $ spin "words" == "sdrow"
  , property $ spin "hello" == "olleh"
  , property $ spin "world" == "dlrow"
  ]

spin :: String -> String
spin w  =  if length w >= 5
           then reverse w
           else w

gps21g :: String -> String
gps21g s  =  unwords (map spin (words s))

gps21c :: IO ()
gps21c  =  do
  conjureFromSpec "spin" spinSpec
    [ fun "length"  (length :: String -> Int)
    , fun "reverse" (reverse :: String -> String)
    , guard
    , fun ">="      ((>=) :: Int -> Int -> Bool)
    , unfun (5 :: Int)
    ]

  conjureFromSpec "gps21_spinwords" gps21s
    [ fun "words"   words
    , fun "unwords" unwords
    , fun "spin"    (spin :: String -> String)
    , fun "map"     (map :: (String -> String) -> [String] -> [String])
    , fun "length"  (length :: String -> Int)
    , fun "reverse" (reverse :: String -> String)
    , guard
    , fun ">="      ((>=) :: Int -> Int -> Bool)
    , unfun (5 :: Int)
    ]


-- PSB2 #22 -- Square Digits (CW) --
-- Given a positive integer, square each digit and
-- concatenate the squares into a returned string.

gps22p :: Int -> String
gps22p 1  =  "1"
gps22p 12  =  "14"
gps22p 21  =  "41"
gps22p 123  =  "149"
gps22p 321  =  "941"
gps22p 1234  =  "14916"
gps22p 4321  =  "16941"

-- unreachable as-is
-- reachable with auxiliary functions: div10, mod10 and square
gps22g :: Int -> String
gps22g 0  =  ""  -- 1
gps22g x  =  gps22g (x `div` 10) ++ show (x `mod` 10 * x `mod` 10)  -- 14
-- gps22g x  =  gps22g (x `div` 10) ++ show ((x `mod` 10) ^ 2)  -- 12

gps22c :: IO ()
gps22c  =  do
  conjure "gps22" gps22p
    [ unfun (0 :: Int)

    -- uncomment the following four for the a runtime of 15s @ size 11
    -- , unfun (10 :: Int)
    -- , fun "`div`" (div :: Int -> Int -> Int)
    -- , fun "`mod`" (mod :: Int -> Int -> Int)
    -- , fun "square" ((^2) :: Int -> Int)
    , fun "div10" ((`div` 10) :: Int -> Int)
    , fun "mod10" ((`mod` 10) :: Int -> Int)
    , fun "*" ((*) :: Int -> Int -> Int)

    , fun "show" (show :: Int -> String)
    , unfun ""
    , fun "++" ((++) :: String -> String -> String)
    ]


gps23s :: (String -> String -> String -> String) -> [Property]
gps23s s  =
  [ property $ s "abcd" "abcd" "abcd" == "abcd"
  , property $ s "abcd" "dcba" "abcd" == "dcba"
  , property $ s "abcd" "1234" "abcd" == "1234"
  , property $ s "abcd" "1234" "bacd" == "2134"
  ]

gps23g :: String -> String -> String -> String
-- gps23g f t s  =  map (\c -> fromJust $ c `lookup` zipWith (,) f t) s
gps23g f t s  =  map (fromJust . (`lookup` zipWith (,) f t)) s

gps23c :: IO ()
gps23c  =  do
  -- cannot conjure, needs lambda
  conjureFromSpec "gps23" gps23s
    [
    ]


data Twitter  =  Tweet Int
              |  TooMany
              |  Empty
              deriving (Eq, Show)

deriveExpress  ''Twitter
deriveListable ''Twitter
deriveName     ''Twitter

instance Conjurable Twitter where
  conjureExpress   =  reifyExpress
  conjureEquality  =  reifyEquality
  conjureTiers     =  reifyTiers

gps24g_twitter :: String -> Twitter
gps24g_twitter ""  =  Empty
gps24g_twitter s  =  if length s > 140
                     then TooMany
                     else Tweet (length s)

gps24s_twitter :: (String -> Twitter) -> [Property]
gps24s_twitter twitter  =
  [ property $ twitter "" == Empty
  , property $ twitter "abcd" == Tweet 4
  , property $ twitter "0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij" == Tweet 140
  , property $ twitter "0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghijX" == TooMany
  ]

gps24c :: IO ()
gps24c  =  do
  conjureFromSpec "gps24" gps24s_twitter
    [ unfun Empty
    , unfun TooMany
    , fun "Tweet" Tweet
    , guard
    , unfun ""
    , fun ":" ((:) :: Char -> String -> String)
    , fun "length" (length :: String -> Int)
    , unfun (140 :: Int)
    , fun ">" ((>) :: Int -> Int -> Bool)
    ]


gps25p :: [Double] -> [Double] -> Double
gps25p [0] [1]  =  1
gps25p [1] [0]  =  1
gps25p [-1] [1]  =  2
gps25p [0,1] [1,0]  =  sqrt 2
gps25p [1,0] [0,1]  =  sqrt 2
gps25p [0,0,1] [1,0,0]  =  sqrt 2
gps25p [0,1,2] [1,2,3]  =  sqrt 3

gps25g :: [Double] -> [Double] -> Double
gps25g v1 v2  =  sqrt (foldr (+) 0 (map (^2) (zipWith (-) v1 v2)))

-- out of reach performance-wise
gps25c :: IO ()
gps25c  =  conjure "gps25" gps25p
  [ unfun (0 :: Double)
  , unfun (2 :: Double)
  , fun "+" ((+) :: Double -> Double -> Double)
  , fun "-" ((-) :: Double -> Double -> Double)
  , fun "**" ((**) :: Double -> Double -> Double)
  , fun "zipWith" (zipWith :: (Double -> Double -> Double) -> [Double] -> [Double] -> [Double])
  , fun "map" (map :: (Double -> Double) -> [Double] -> [Double])
  , fun "sqrt" (sqrt :: Double -> Double)
  , maxSize 6
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
         , gps13c
         , gps14c
         , gps15c
         , gps16c
         , gps17c
         , gps18c
         , gps19c
         , gps20c
         , gps21c
         , gps22c
         , gps23c
         , gps24c
         , gps25c
         ]
