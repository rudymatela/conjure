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

min' :: Int -> Int -> Int -> Int
min' 0 1 2  =  0
min' 0 2 1  =  0
min' 2 3 1  =  1
min' 1 0 2  =  0
min' 4 3 1  =  1
min' 4 1 3  =  1

max' :: Int -> Int -> Int -> Int
max' 0 1 2  =  2
max' 0 2 1  =  2
max' 2 3 1  =  3
max' 1 0 2  =  2
max' 4 3 1  =  4
max' 4 1 3  =  4

median' :: Int -> Int -> Int -> Int
median' 0 1 2  =  1
median' 0 2 1  =  1
median' 2 3 1  =  2
median' 1 0 2  =  1
median' 4 3 1  =  3
median' 4 1 3  =  3

main :: IO ()
main = do
  conjure "negate"  negate' ingredients
  conjure "abs"     abs'    ingredients
  conjure "signum"  signum' ingredients
  conjure "compare" compare' compareIngredients

  conjure "min" min' mmmIngredients
  conjure "max" max' mmmIngredients
  -- median is unreachable performance-wise
  -- conjure "median" median' mmmIngredients

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

mmmIngredients :: [Ingredient]
mmmIngredients  =
  [ fun "<=" ((<=) :: Int -> Int -> Bool)
  , iif (undefined :: Int)
  , target 50400
  ]
