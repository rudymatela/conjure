-- self.hs: conjuring functions through themselves
--
-- Copyright (C) 2021-2025 Rudy Matela
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
import Conjure

main :: IO ()
main = do
  cj "?" ((+) :: Int -> Int -> Int)   ingredients
  cj "?" ((*) :: Int -> Int -> Int)   ingredients
  cj "i" ((+1) :: Int -> Int)         ingredients
  cj "d" ((subtract 1) :: Int -> Int) ingredients
  where
  -- the monomorphism restriction strikes again
  cj :: Conjurable f => String -> f -> [Ingredient] -> IO ()
  cj  =  conjureWith args{maxSize=3,maxEquationSize=0}

ingredients :: [Ingredient]
ingredients =
  [ con (0::Int)
  , con (1::Int)
  , fun "+" ((+) :: Int -> Int -> Int)
  , fun "*" ((*) :: Int -> Int -> Int)
  ]
