-- arith.hs: conjuring simple non-recursive numeric functions
--
-- Copyright (C) 2021 Rudy Matela
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
import Conjure

double :: Int -> Int
double 0  =  0
double 1  =  2
double 2  =  4
double 3  =  6

triple :: Int -> Int
triple 0  =  0
triple 1  =  3
triple 2  =  6
triple 3  =  9

add :: Int -> Int -> Int
add 0 0  =  0
add 0 1  =  1
add 1 0  =  1
add 1 1  =  2

square :: Int -> Int
square 0  =  0
square 1  =  1
square 2  =  4

cube :: Int -> Int
cube 0  =  0
cube 1  =  1
cube 2  =  8

tnpo :: Int -> Int
tnpo 0  =  1
tnpo 1  =  4
tnpo 2  =  7

main :: IO ()
main = do
  conjure "double" double primitives
  conjure "triple" triple primitives
  conjure "add"    add    primitives
  conjure "square" square primitives
  conjure "cube"   cube   primitives
  conjure "tnpo"   tnpo   primitives

primitives :: [Prim]
primitives =
  [ pr (0::Int)
  , pr (1::Int)
  , prim "+" ((+) :: Int -> Int -> Int)
  , prim "*" ((*) :: Int -> Int -> Int)
  ]
