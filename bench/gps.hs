-- gps.hs: General Program Synthesis Benchmark Suite
--
-- Copyright (C) 2021 Rudy Matela
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
import Conjure
import System.Environment (getArgs)

import Data.Char (isLetter)                      -- GPS bench  #5
import Data.Char (isSpace)                       -- GPS bench  #7
import Data.Ratio ((%), numerator, denominator)  -- GPS bench #10


gps1p :: Int -> Float -> Float
gps1p 0 1.0  =  1.0
gps1p 1 0.0  =  1.0
gps1p 1 1.0  =  2.0
gps1p 1 1.5  =  2.5

gps1g :: Int -> Float -> Float
gps1g x f  =  fromIntegral x + f

gps1c :: IO ()
gps1c  =  conjure "gps1" gps1p
  [ prim "+" ((+) :: Float -> Float -> Float)
  , prim "fromIntegral" (fromIntegral :: Int -> Float)
  ]


gps2p :: Int -> Maybe String
gps2p    0  =  Just "small"
gps2p  500  =  Just "small"
gps2p 1000  =  Nothing
gps2p 1500  =  Nothing
gps2p 2000  =  Just "large"
gps2p 2500  =  Just "large"

gps2g :: Int -> Maybe String
gps2g n
  | n <  1000  =  Just "small"
  | 2000 <= n  =  Just "large"
  | otherwise  =  Nothing

gps2c :: IO ()
gps2c  =  conjureWith args{maxTests=5080, maxSize=30} "gps2" gps2p
  [ pr "small"
  , pr "large"
  , pr (1000 :: Int)
  , pr (2000 :: Int)
  , prim "Just" (Just :: String -> Maybe String)
  , prim "Nothing" (Nothing :: Maybe String)
  , prim "<=" ((<=) :: Int -> Int -> Bool)
  , prim "<" ((<) :: Int -> Int -> Bool)
  , prif (undefined :: Maybe String)
  ]


gps3p :: Int -> Int -> Int -> [Int]
gps3p 0 9 1  =  [0,1,2,3,4,5,6,7,8]
gps3p 2 9 2  =  [2,4,6,8]

gps3g1 :: Int -> Int -> Int -> [Int]
gps3g1 start end step  =  enumFromThenTo start (step+start) (end-1)

gps3g2 :: Int -> Int -> Int -> [Int]
gps3g2 start end step  =  if start < end
                          then start : gps3g2 (start+step) end step
                          else []

gps3c :: IO ()
gps3c  =  do
  conjure "gps3" gps3p
    [ pr (1 :: Int)
    , prim "enumFromThenTo" ((\x y z -> take 720 $ enumFromThenTo x y z) :: Int -> Int -> Int -> [Int])
    , prim "+" ((+) :: Int -> Int -> Int)
    , prim "-" ((-) :: Int -> Int -> Int)
    ]

  -- not possible, no recursive descent
  conjureWith args{maxSize=8} "gps3" gps3p
    [ pr ([] :: [Int])
    , prim ":" ((:) :: Int -> [Int] -> [Int])
    , prim "+" ((+) :: Int -> Int -> Int)
    , prim "<" ((<) :: Int -> Int -> Bool)
    , prif (undefined :: [Int])
    ]


gps4p :: String -> String -> String -> Bool
gps4p "" "a" "aa"  =  True
gps4p "aa" "a" ""  =  False
gps4p "a" "aa" ""  =  False
gps4p "a" "aa" "aaa"  =  True
gps4p "a" "aaa" "aa"  =  False
gps4p "aa" "a" "aaa"  =  False
gps4p "aa" "aaa" "a"  =  False
gps4p "aaa" "a" "aa"  =  False
gps4p "aaa" "aa" "a"  =  False

gps4g :: String -> String -> String -> Bool
gps4g s1 s2 s3  =  length s1 < length s2 && length s2 < length s3

gps4c :: IO ()
gps4c  =  do
  conjure "gps4" gps4p
    [ prim "length" (length :: String -> Int)
    , prim "<" ((<) :: Int -> Int -> Bool)
    , prim "&&" (&&)
    ]


gps5p :: String -> String
gps5p "a"  =  "aa"
gps5p "b"  =  "bb"
gps5p " "  =  " "
gps5p "!"  =  "!!!"
gps5p "aa"  =  "aaaa"

gps5g :: String -> String
gps5g []  =  []
gps5g (c:cs)
  | isLetter c  =  c:c:gps5g cs
  | c == '!'    =  c:c:c:gps5g cs
  | otherwise   =  c:gps5g cs

gps5c :: IO ()
gps5c  =  conjureWith args{maxSize=6} "gps5" gps5p -- can't find
  [ pr ""
  , prim ":" ((:) :: Char -> String -> String)
  , pr '!'
  , prim "==" ((==) :: Char -> Char -> Bool)
  , prim "isLetter" (isLetter :: Char -> Bool)
  , prif (undefined :: String -> String)
  ]


-- GPS Benchmark #6 -- Collatz/Hailstone numbers --

gps6p :: Int -> Int
gps6p 1  =  1
gps6p 2  =  2
gps6p 3  =  8
gps6p 4  =  3
gps6p 5  =  6
gps6p 6  =  9
gps6p 12  =  10
gps6p 60  =  20
gps6p 360  =  20

gps6g :: Int -> Int
gps6g  =  tnp1
  where
  tnp1 n | n <= 0  =  undefined
  tnp1 1  =  1                          --  1
  tnp1 n  =  1 + gps6g (if even n       --  7
                        then n `div` 2  -- 10
                        else 3*n + 1)   -- 15

-- This one is out of reach performance wise:
-- Speculate hangs with this background.
-- Removing three or setting maxEqSize to 4 makes it unhang.
-- But a size of 15 or 17 is simplyl out of our reach.
gps6c :: IO ()
gps6c  =  conjureWith args{maxSize=6,maxEquationSize=3} "gps6" gps6p
  [ pr (1 :: Int)
  , pr (2 :: Int)
  , pr (3 :: Int)
  , prim "+" ((+) :: Int -> Int -> Int)
  , prim "*" ((*) :: Int -> Int -> Int)
  , prim "`div`" (div :: Int -> Int -> Int)
  , prim "even" (even :: Int -> Bool)
  , prif (undefined :: Int)
  ]


-- GPS Benchmark #7 -- Replace Space with Newline (P 4.3)

gps7p :: String -> (String, Int)
gps7p "a"  =  ("a", 1)
gps7p "aa"  =  ("aa", 2)
gps7p "a a"  =  ("a\na", 2)
gps7p "a\na"  =  ("a\na", 2)

gps7g :: String -> (String, Int)
gps7g s  =  (init $ unlines $ words s, length (filter (not . isSpace) s))

gps7c :: IO ()
gps7c  =  conjure "gps7" gps7p
  [ prim "," ((,) :: String -> Int -> (String, Int))
  , prim "init" (init :: String -> String)
  , prim "unlines" unlines
  , prim "words" words
  , prim "length" (length :: String -> Int)
  , prim "filter" (filter :: (Char -> Bool) -> String -> String)
  , prim "not" not
  , prim "." ((.) :: (Bool -> Bool) -> (Char -> Bool) -> Char -> Bool) -- cheat?
  , prim "isSpace" (isSpace :: Char -> Bool)
  ]


-- GPS Benchmark #8 -- String Differences

gps8p :: String -> String -> [(Int, Char, Char)]
gps8p "a" "a"  =  []
gps8p "a" "b"  =  [(0,'a','b')]
gps8p "aa" "ab"  =  [(1,'a','b')]
gps8p "dealer" "dollar"  =  [(1,'e','o'), (2,'a','l'),(4,'e','a')]

gps8g :: String -> String -> [(Int, Char, Char)]
gps8g  =  diffs 0
  where
  diffs _ [] _  =  []
  diffs _ _ []  =  []
  diffs n (c:cs) (d:ds)  =  if c == d
                            then diffs (n+1) cs ds
                            else (n,c,d) : diffs (n+1) cs ds

-- out of reach as Conjure cannot invent helper functions
-- even if that would be solved,
-- I conjecture it would be out-of-reach performance-wise.
gps8c :: IO ()
gps8c  =  conjure "gps8" gps8p
  [
  ]


-- GPS Benchmark #9 -- Even Squares
-- given an integer _n_, print all of the positive even perfect squares less
-- than _n_ on separate lines.

gps9p :: Int -> [Int]
gps9p 10  =  [4]
gps9p 100  =  [4,16,36,64]
gps9p 1000  =  [4,16,36,64,100,144,196,256,324,400,484,576,676,784,900]

-- non-optimal performance, but does the job
-- gps9g :: Int -> [Int]
-- gps9g n  =  [x*x | x <- [1..n], x*x < n, even (x*x)]
gps9g :: Int -> [Int]
gps9g n  =  filter (n >) (filter even (map sq [1..n]))
  where
  sq  =  (^2)

gps9c :: IO ()
gps9c  =  conjureWith args{maxTests=60} "gps9" gps9p
  [ pr (1 :: Int)
  , prim "map" (map :: (Int -> Int) -> [Int] -> [Int])
  , prim "filter" (filter :: (Int -> Bool) -> [Int] -> [Int])
  , prim ".." (enumFromTo :: Int -> Int -> [Int])
  , prim ">" ((>) :: Int -> Int -> Bool)
  , prim "even" (even :: Int -> Bool)
  , prim "sq" ((^2) :: Int -> Int)  -- invented separately
  ]


-- GPS Benchmark #10 -- Wallis Pi
-- (quarter pi approximation)
-- 2   4   4   6   6   8   8
-- - x - x - x - x - x - x - x ...
-- 3   3   5   5   7   7   9

gps10p :: Int -> Rational
gps10p 1  =     2/3
gps10p 2  =     8/9
gps10p 3  =    32/45
gps10p 4  =    64/75
gps10p 5  =   128/175
gps10p 6  =  1024/1225

gps10g :: Int -> Rational
gps10g n  =  product $ take n $ iterate wallisNext (2/3)

wallisNextP :: Rational -> Rational
wallisNextP q
  | q == 2/3  =  4/3
  | q == 4/3  =  4/5
  | q == 4/5  =  6/5
  | q == 6/5  =  6/7
  | q == 6/7  =  8/7
  | q == 8/7  =  8/9

wallisNext :: Rational -> Rational
wallisNext q  =  if n < d
                 then (n+2) % d
                 else n % (d+2)
  where
  n  =  numerator q
  d  =  denominator q
-- wallisNext (x % y)  =  (y + (y + 2)) % (x + (x + 2)) -- which simplifies to...
-- wallisNext (x % y)  =  (x + x * y) % (x + x * x)     -- which simplifies to...
-- wallisNext (x % y)  =  (y + 1) % (x + 1)             -- this correct version


gps10c :: IO ()
gps10c  =  do
  conjureWith args{maxSize=14} "wallisNext" wallisNextP
    [ pr (1 :: Integer)
    , pr (2 :: Integer)
    , prim "+" ((+) :: Integer -> Integer -> Integer)
    , prim "*" ((*) :: Integer -> Integer -> Integer)
    , prim "%" ((%) :: Integer -> Integer -> Rational)
    , prim "<" ((<) :: Integer -> Integer -> Bool)
--  , prim "numerator" (numerator :: Rational -> Integer)
--  , prim "denominator" (denominator :: Rational -> Integer)
    , prif (undefined :: Rational)
    ]

  conjure "gps10" gps10p
    [ pr (2 :: Integer)
    , pr (3 :: Integer)
    , prim "%" ((%) :: Integer -> Integer -> Rational)
--  , pr (2/3 :: Rational)
    , prim "product"    (product :: [Rational] -> Rational)
    , prim "take"       (take :: Int -> [Rational] -> [Rational])
    , prim "iterate"    ((\f -> take 720 . iterate f) :: (Rational -> Rational) -> Rational -> [Rational])
    , prim "wallisNext" wallisNext
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
         ]
