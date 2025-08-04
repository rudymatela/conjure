-- conditionals.hs: assorted functions involving conditionals
--
-- Copyright (C) 2021-2025 Rudy Matela
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
import Conjure

negate' :: Int -> Int
negate' 1  =  -1
negate' 2  =  -2
negate' (-1)  =   1
negate' (-2)  =   2

abs' :: Int -> Int
abs' 1  =  1
abs' 2  =  2
abs' (-1)  =  1
abs' (-2)  =  2

signum' :: Int -> Int
signum' 0  =  0
signum' 1  =  1
signum' 2  =  1
signum' 3  =  1
signum' (-1)  =  -1
signum' (-2)  =  -1
signum' (-3)  =  -1

compare' :: Int -> Int -> Ordering
compare' 0 0  =  EQ
compare' 1 1  =  EQ
compare' 2 2  =  EQ
compare' 0 1  =  LT
compare' 1 2  =  LT
compare' 1 0  =  GT
compare' 2 1  =  GT
compare' (-1) 0  =  LT
compare' 0 (-1)  =  GT

main :: IO ()
main = do
  conjure "negate"  negate' ingredients
  conjure "abs"     abs'    ingredients
  conjure "signum"  signum' ingredients
  conjure "compare" compare' compareIngredients

ingredients :: [Ingredient]
ingredients  =
  [ unfun (0::Int)
  , unfun (1::Int)
  , unfun (-1::Int)
  , fun "+" ((+) :: Int -> Int -> Int)
  , fun "-" ((-) :: Int -> Int -> Int)
  , fun "*" ((*) :: Int -> Int -> Int)
  , fun "<"  ((<)  :: Int -> Int -> Bool)
  , guard
  ]

compareIngredients :: [Ingredient]
compareIngredients  =
  [ unfun EQ
  , unfun LT
  , unfun GT
  , fun "==" ((==) :: Int -> Int -> Bool)
  , fun "<"  ((<)  :: Int -> Int -> Bool)
--, fun "<=" ((<=) :: Int -> Int -> Bool)
  , guard
  ]
