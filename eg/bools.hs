-- bools.hs: simple recursive functions over boolean lists
--
-- Copyright (C) 2021-2024 Rudy Matela
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

-- target (for and):

-- and ps  =  if null ps then True else head ps && and (tail ps)
--            1  2    3       4         5    6  7  8    9    10 symbols
-- or
-- and ps  =  null ps || head ps && and (tail ps)
--            1    2  3  4    5  6  7    8    9 symbols
--
--
-- for or, we only reach the solution at size 11:
--
-- or' ps  =  not (null ps || not (head ps || or' (tail ps)))
--
-- unfortunately this is an ill-case,
-- I think there are smaller expressions that rewrite to the above bigger
-- expression so it is only found at the bigger size.
