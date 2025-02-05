-- self.hs: conjuring functions through themselves
--
-- Copyright (C) 2021-2025 Rudy Matela
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
import Conjure

main :: IO ()
main = do
  cj "?" ((+) :: Int -> Int -> Int)   primitives
  cj "?" ((*) :: Int -> Int -> Int)   primitives
  cj "i" ((+1) :: Int -> Int)         primitives
  cj "d" ((subtract 1) :: Int -> Int) primitives
  where
  -- the monomorphism restriction strikes again
  cj :: Conjurable f => String -> f -> [Prim] -> IO ()
  cj  =  conjureWith args{maxSize=3,maxEquationSize=0}

primitives :: [Prim]
primitives =
  [ pr (0::Int)
  , pr (1::Int)
  , prim "+" ((+) :: Int -> Int -> Int)
  , prim "*" ((*) :: Int -> Int -> Int)
  ]
