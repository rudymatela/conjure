-- oddeven.hs: conjuring even and odd from two sets of primitives
--
-- Copyright (C) 2024 Rudy Matela
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
  conjure "odd"  odd  primitives1
  conjure "even" even primitives1
  conjure "odd"  odd  primitives2
  conjure "even" even primitives2

primitives1 :: [Prim]
primitives1 =
  [ pr (0::Int)
  , pr (1::Int)
  , pr (2::Int)
  , prim "+" ((+) :: Int -> Int -> Int)

  , prim "-" ((-) :: Int -> Int -> Int)
  , pr False
  , pr True
  ]

primitives2 :: [Prim]
primitives2 =
  [ pr (0::Int)
  , pr (1::Int)
  , pr (2::Int)
  , prim "+" ((+) :: Int -> Int -> Int)

  , prim "`mod`" (mod :: Int -> Int -> Int)
  , prim "==" ((==) :: Int -> Int -> Bool)
  ]
