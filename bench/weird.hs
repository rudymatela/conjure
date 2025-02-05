-- weird.hs: conjures some weird or unintuitive functions
--
-- Copyright (C) 2025 Rudy Matela
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
--
-- This file conjures some weird or unintuitive functions.
-- It is here to make sure we do not overprune!
--
-- For simplicity,
-- we do not use partial definitions here.
-- We use fully defined functions to conjure themselves.
import Conjure

-- | xor for integers
--
-- returns the sum but only when one of the arguments is 0
(^^^) :: Int -> Int -> Int
0 ^^^ y  =  y
x ^^^ 0  =  x
_ ^^^ _  =  0

main :: IO ()
main  =  do
  conjure "^^^" (^^^) primitives
  conjureWith args{usePatterns = False} "^^^" (^^^) primitives

  -- This example is quite degenerate,
  -- it takes a while to conjure even with just 2 primitives.
  -- I am leaving it commented-out for now...
  -- conjure "thirty" thirty [pr (0::Int), prim "+" ((+) :: Int -> Int -> Int)]

primitives :: [Prim]
primitives  =
  [ pr (0::Int)
  , pr (1::Int)
  , prim "+" ((+) :: Int -> Int -> Int)
  , prim "*" ((*) :: Int -> Int -> Int)
  , prim "==" ((==) :: Int -> Int -> Bool)
  , prim "&&" (&&)
  , prim "||" (||)
  , prif (undefined :: Int)
  ]

-- | returns the sum when one of the arguments is 0
--
-- Naming: thirty/30, sum of __3__, when one is __0__
thirty :: Int -> Int -> Int -> Int
thirty 0 y z  =  y + z
thirty x 0 z  =  x + z
thirty x y 0  =  x + y
thirty x y z  =  0
