-- oddeven.hs: conjuring even and odd from two sets of ingredients
--
-- Copyright (C) 2025 Rudy Matela
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
import Conjure
import Prelude hiding (odd, even)

odd :: Int -> Bool
odd 0  =  False
odd 1  =  True
odd 2  =  False
odd 3  =  True
odd 4  =  False
odd 5  =  True

even :: Int -> Bool
even 0  =  True
even 1  =  False
even 2  =  True
even 3  =  False
even 4  =  True
even 5  =  False

main :: IO ()
main = do
  conjure "odd"  odd  ingredients1
  conjure "even" even ingredients1
  conjure "odd"  odd  ingredients2
  conjure "even" even ingredients2

ingredients1 :: [Ingredient]
ingredients1 =
  [ unfun (0::Int)
  , unfun (1::Int)
  , unfun (2::Int)
  , fun "+" ((+) :: Int -> Int -> Int)

  , fun "-" ((-) :: Int -> Int -> Int)
  , unfun False
  , unfun True
  ]

ingredients2 :: [Ingredient]
ingredients2 =
  [ unfun (0::Int)
  , unfun (1::Int)
  , unfun (2::Int)
  , fun "+" ((+) :: Int -> Int -> Int)

  , fun "`mod`" (mod :: Int -> Int -> Int)
  , fun "==" ((==) :: Int -> Int -> Bool)
  ]
