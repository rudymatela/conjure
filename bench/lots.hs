-- A big set of background primitives, will Conjure scale?
--
-- Copyright (C) 2021 Rudy Matela
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
import Conjure

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
  conjure "factorial n" factorial primitives
--conjure "sum"     (sum     :: [Int] -> Int) primitives
--conjure "product" (product :: [Int] -> Int) primitives
--conjure "length"  (length  :: [Int] -> Int) primitives
--conjure "count" count' (prim "length" (length :: [Int] -> Int):primitives)

primitives :: [Prim]
primitives  =
  [ pr False
  , pr True
  , prim "&&" (&&)
  , prim "||" (||)
  , prim "not" not

  , pr (0::Int)
  , pr (1::Int)
  , prim "+" ((+) :: Int -> Int -> Int)
  , prim "*" ((*) :: Int -> Int -> Int)
  , prim "dec" (subtract 1 :: Int -> Int)
  , prim "-" ((-) :: Int -> Int -> Int)

  , prim "==" ((==) :: Int -> Int -> Bool)
  , prim "<=" ((<=) :: Int -> Int -> Bool)
  , prim "<"  ((<) :: Int -> Int -> Bool)

  , prim "const" (const :: Int -> Int -> Int)

  , pr ([] :: [Int])
  , prim ":" ((:) :: Int -> [Int] -> [Int])
  , prim "head" (head :: [Int] -> Int)
  , prim "tail" (tail :: [Int] -> [Int])
  , prim "null" (null :: [Int] -> Bool)
  , prim "foldr" (foldr :: (Int -> Int -> Int) -> Int -> [Int] -> Int)

  , prim "map" (map :: (Int -> Int) -> [Int] -> [Int])
  , prim "filter" (filter :: (Int -> Bool) -> [Int] -> [Int])
  , prim ".." (enumFromTo :: Int -> Int -> [Int])

  , prim "++" ((++) :: [Int] -> [Int] -> [Int])
  , prim "elem" (elem :: Int -> [Int] -> Bool)
  ]
