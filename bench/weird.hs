-- weird.hs: conjures some weird or unintuitive functions
--
-- Copyright (C) 2024 Rudy Matela
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
--
-- This file conjures some weird or unintuitive functions.
-- It is here to make sure we do not overprune!
import Conjure

-- | xor for integers
--
-- returns the sum but only when one of the arguments is 0
(^^^) :: Int -> Int -> Int
0 ^^^ y  =  y
x ^^^ 0  =  x
_ ^^^ _  =  0

main :: IO ()
main = do
  conjure "^^^" (^^^) primitives
  conjureWith args{usePatterns = False} "^^^" (^^^) primitives

primitives :: [Prim]
primitives =
  [ pr (0::Int)
  , pr (1::Int)
  , prim "+" ((+) :: Int -> Int -> Int)
  , prim "*" ((*) :: Int -> Int -> Int)
  , prim "==" ((==) :: Int -> Int -> Bool)
  , prim "&&" (&&)
  , prim "||" (||)
  , prif (undefined :: Int)
  ]
