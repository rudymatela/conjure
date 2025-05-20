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
  conjure "and" (and' :: [Bool] -> Bool) ingredients
  conjure "or"  (or'  :: [Bool] -> Bool) ingredients

  -- conjure can use fold as well
  conjure "and" (and' :: [Bool] -> Bool) ingredientsWithFold
  conjure "or"  (or'  :: [Bool] -> Bool) ingredientsWithFold

ingredients :: [Ingredient]
ingredients =
  [ unfun False
  , unfun True
  , fun "not" not
  , fun "||" (||)
  , fun "&&" (&&)
  ]

ingredientsWithFold :: [Ingredient]
ingredientsWithFold  =
    fun "foldr" (foldr :: (Bool -> Bool -> Bool) -> Bool -> [Bool] -> Bool)
  : ingredients
