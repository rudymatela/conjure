-- bools.hs: simple recursive functions over boolean lists
--
-- Copyright (C) 2021-2025 Rudy Matela
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
import Conjure

and' :: [Bool] -> Bool
and' [p]      =  p
and' [p,q]    =  p && q
and' [p,q,r]  =  p && q && r

or' :: [Bool] -> Bool
or' [p]      =  p
or' [p,q]    =  p || q
or' [p,q,r]  =  p || q || r

main :: IO ()
main = do
  conjure "and" (and' :: [Bool] -> Bool) primitives
  conjure "or"  (or'  :: [Bool] -> Bool) primitives

  -- conjure can use fold as well
  conjure "and" (and' :: [Bool] -> Bool) primitivesWithFold
  conjure "or"  (or'  :: [Bool] -> Bool) primitivesWithFold

primitives :: [Prim]
primitives =
  [ pr False
  , pr True
  , prim "not" not
  , prim "||" (||)
  , prim "&&" (&&)
  , prim "null" (null :: [Bool] -> Bool)
  , prim "head" (head :: [Bool] -> Bool)
  , prim "tail" (tail :: [Bool] -> [Bool])
  ]

primitivesWithFold :: [Prim]
primitivesWithFold  =
    prim "foldr" (foldr :: (Bool -> Bool -> Bool) -> Bool -> [Bool] -> Bool)
  : primitives
