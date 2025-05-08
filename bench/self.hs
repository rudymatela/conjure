-- self.hs: conjuring functions through themselves
--
-- Copyright (C) 2021-2025 Rudy Matela
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
import Conjure

main :: IO ()
main = do
  conjure "?" ((+) :: Int -> Int -> Int)   ingredients
  conjure "?" ((*) :: Int -> Int -> Int)   ingredients
  conjure "i" ((+1) :: Int -> Int)         ingredients
  conjure "d" ((subtract 1) :: Int -> Int) ingredients

ingredients :: [Ingredient]
ingredients =
  [ con (0::Int)
  , con (1::Int)
  , fun "+" ((+) :: Int -> Int -> Int)
  , fun "*" ((*) :: Int -> Int -> Int)
  , maxSize 3
  , maxEquationSize 0
  ]
