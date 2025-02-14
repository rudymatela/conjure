-- peano.hs: conjuring functions over peano naturals
--
-- This is a minimal example demonstrating user-defined data types.
--
-- Copyright (C) 2025 Rudy Matela
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
{-# LANGUAGE TemplateHaskell #-}
import Conjure
import Test.LeanCheck

data Peano  =  Z | S Peano
  deriving (Show, Eq)

-- The following not only derives a Conjurable instance
-- but also the needed Listable, Express and Name instances.
deriveConjurable ''Peano

plus :: Peano -> Peano -> Peano
plus Z Z  =  Z
plus Z (S Z)  =  S Z
plus (S Z) Z  =  S Z
plus (S Z) (S Z)  =  S (S Z)
plus (S (S Z)) (S Z)  =  S (S (S Z))
plus (S Z) (S (S Z))  =  S (S (S Z))

main :: IO ()
main  =  do
  conjure "+" plus
    [ pr Z
    , prim "S" S
    ]

  -- use + to conjure *
  conjure "*" times
    [ pr Z
    , prim "S" S
    , prim "+" (let p + Z    =  p
                    p + S q  =  S p + q
                in (+))
    ]

times :: Peano -> Peano -> Peano
times Z Z  =  Z
times Z (S Z)  =  Z
times (S Z) Z  =  Z
times (S Z) (S Z)  =  S Z
times (S (S Z)) (S Z)  =  (S (S Z))
times (S Z) (S (S Z))  =  (S (S Z))
times (S (S Z)) (S (S Z))  =  (S (S (S (S Z))))
