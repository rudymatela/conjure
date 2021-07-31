-- gps.hs: General Program Synthesis Benchmark Suite
--
-- Copyright (C) 2021 Rudy Matela
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
import Conjure
import System.Environment (getArgs)

import Data.Char (isLetter) -- GPS #5


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
    , prim "enumFromThenTo" ((\x y z -> take 360 $ enumFromThenTo x y z) :: Int -> Int -> Int -> [Int])
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
         ]
