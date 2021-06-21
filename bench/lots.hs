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


main :: IO ()
main  =  do
  putStrLn $ "running with " ++ show (length primitives) ++ " primitives"
  conjure "factorial n" factorial primitives

primitives :: [Expr]
primitives  =
  [ val False
  , val True
  , value "&&" (&&)
  , value "||" (||)
  , value "not" not

  , val (0::Int)
  , val (1::Int)
  , value "+" ((+) :: Int -> Int -> Int)
  , value "*" ((*) :: Int -> Int -> Int)
  , value "dec" (subtract 1 :: Int -> Int)
  , value "-" ((-) :: Int -> Int -> Int)

  , value "==" ((==) :: Int -> Int -> Bool)
  , value "<=" ((<=) :: Int -> Int -> Bool)
  , value "<"  ((<) :: Int -> Int -> Bool)

  , val ([] :: [Int])
  , value ":" ((:) :: Int -> [Int] -> [Int])
  , value "head" (head :: [Int] -> Int)
  , value "tail" (tail :: [Int] -> [Int])
  , value "null" (null :: [Int] -> Bool)
  , value "foldr" (foldr :: (Int -> Int -> Int) -> Int -> [Int] -> Int)

  , value "map" (map :: (Int -> Int) -> [Int] -> [Int])
  , value "filter" (filter :: (Int -> Bool) -> [Int] -> [Int])
  , value ".." (enumFromTo :: Int -> Int -> [Int])

  , value "++" ((++) :: [Int] -> [Int] -> [Int])
  , value "elem" (elem :: Int -> [Int] -> Bool)
  ]
