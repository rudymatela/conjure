-- 30 background ingredients, does Conjure scale?
--
-- Copyright (C) 2021-2025 Rudy Matela
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
import Conjure
import System.Environment

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
  putStrLn $ "running with " ++ show (length ingredients) ++ " ingredients"
  as <- getArgs
  case as of
    ["factorial"]     -> conjure "factorial n" factorial   ingredients
    ["factorial","t"] -> conjure "factorial n" factorial $ ingredients ++ [maxSize 1]
    ["sum"]           -> conjure "sum"     (sum     :: [Int] -> Int)   ingredients
    ["sum","t"]       -> conjure "sum"     (sum     :: [Int] -> Int) $ ingredients ++ [maxSize 1]
    ["product"]       -> conjure "product" (product :: [Int] -> Int)   ingredients
    ["product","t"]   -> conjure "product" (product :: [Int] -> Int) $ ingredients ++ [maxSize 1]
    ["length"]        -> conjure "length"  (length  :: [Int] -> Int)   ingredients
    ["length","t"]    -> conjure "length"  (length  :: [Int] -> Int) $ ingredients ++ [maxSize 1]
    ["count"]         -> conjure "count"   count' $ ingredients ++ primsLength
    ["count","t"]     -> conjure "count"   count' $ ingredients ++ primsLength ++ [maxSize 1]
    _                 -> putStrLn "usage: p30 <factorial|sum|product|length|count> [t]"

ingredients :: [Ingredient]
ingredients  =
  [ con False
  , con True
  , fun "&&" (&&)
  , fun "||" (||)
  , fun "not" not

  , con (0::Int)
  , con (1::Int)
  , fun "+" ((+) :: Int -> Int -> Int)
  , fun "*" ((*) :: Int -> Int -> Int)
  , fun "dec" (subtract 1 :: Int -> Int)
  , fun "-" ((-) :: Int -> Int -> Int)

  , fun "==" ((==) :: Int -> Int -> Bool)
  , fun "<=" ((<=) :: Int -> Int -> Bool)
  , fun "<"  ((<) :: Int -> Int -> Bool)

  , fun "const" (const :: Int -> Int -> Int)

  , con ([] :: [Int])
  , fun ":" ((:) :: Int -> [Int] -> [Int])
  , fun "head" (head :: [Int] -> Int)
  , fun "tail" (tail :: [Int] -> [Int])
  , fun "null" (null :: [Int] -> Bool)
  , fun "foldr" (foldr :: (Int -> Int -> Int) -> Int -> [Int] -> Int)

  , fun "map" (map :: (Int -> Int) -> [Int] -> [Int])
  , fun "filter" (filter :: (Int -> Bool) -> [Int] -> [Int])
  , fun ".." (enumFromTo :: Int -> Int -> [Int])

  , fun "++" ((++) :: [Int] -> [Int] -> [Int])
  , fun "elem" (elem :: Int -> [Int] -> Bool)
  ]

primsLength :: [Ingredient]
primsLength =
  [ fun "length" (length :: [Int] -> Int)
  ]
