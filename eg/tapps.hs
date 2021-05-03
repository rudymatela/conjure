-- tapps.hs: conjure with type applications
--
-- Copyright (C) 2021 Rudy Matela
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
{-# LANGUAGE TypeApplications #-}

import Conjure

third :: [Int] -> Int
third [x,y,z]  =  z
third [x,y,z,w]  =  z

product' :: [Int] -> Int
product' [x]      =  x
product' [x,y]    =  x*y
product' [x,y,z]  =  x*y*z

main :: IO ()
main = do
  conjure               "third"   third    primitives
  conjureWithMaxSize 10 "product" product' primitives
  conjureWithMaxSize 4  "product" product' primitivesWithFold

primitives :: [Expr]
primitives =
  [ val (0 :: Int)
  , val (1 :: Int)
  , value "+" ((+) @Int)
  , value "*" ((*) @Int)
  , value "null" (null @[] @Int)
  , value "head" (head @Int)
  , value "tail" (tail @Int)
  ]

primitivesWithFold :: [Expr]
primitivesWithFold  =  value "foldr" (foldr @[] @Int @Int) : primitives

-- Some notes:
--
-- This works down to GHC 7.8 even though at the time (base 4.7.0.2) null and
-- foldr were bound to the list type.
--
-- base for GHC 7.8:  https://hackage.haskell.org/package/base-4.7.0.2/docs/Prelude.html#v:null
-- base for GHC 7.10: https://hackage.haskell.org/package/base-4.8.0.0/docs/Prelude.html#v:null
-- working run:       https://github.com/rudymatela/conjure/runs/2493232850
