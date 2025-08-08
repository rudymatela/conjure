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

-- | inc or square based on boolean argument
--   except for 0 where the answer is swapped
isq :: Bool -> Int -> Int
isq False 0  =  0
isq True  0  =  1
isq False x  =  x + 1
isq True  x  =  x * x

main :: IO ()
main  =  do
  conjure "isq" isq ingredients

  conjure "^^^" (^^^)   ingredients
  conjure "^^^" (^^^) $ ingredients ++ [singlePattern]

  -- This example is quite the degenerate case,
  -- it takes a while to conjure even with just 2 ingredients.
  -- I am leaving it commented-out for now...
  -- conjure "thirty" thirty [pr (0::Int), fun "+" ((+) :: Int -> Int -> Int)]

ingredients :: [Ingredient]
ingredients  =
  [ con (0::Int)
  , con (1::Int)
  , fun "+" ((+) :: Int -> Int -> Int)
  , fun "*" ((*) :: Int -> Int -> Int)
  , fun "==" ((==) :: Int -> Int -> Bool)
  , fun "&&" (&&)
  , fun "||" (||)
  , iif (undefined :: Int)
  -- guard does not play well with usePatterns = False yet
  ]

-- | returns the sum when one of the arguments is 0
--
-- Naming: thirty/30, sum of __3__, when one is __0__
thirty :: Int -> Int -> Int -> Int
thirty 0 y z  =  y + z
thirty x 0 z  =  x + z
thirty x y 0  =  x + y
thirty x y z  =  0
