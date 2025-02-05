-- |
-- Module      : Conjure
-- Copyright   : (c) 2021-2025 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- A library for Conjuring function implementations
-- from tests or partial definitions.
-- (a.k.a.: functional inductive programming)
--
-- Step 1: declare your partial function
--
-- > factorial :: Int -> Int
-- > factorial 2  =  2
-- > factorial 3  =  6
-- > factorial 4  =  24
--
-- Step 2: declare a list with the potential building blocks:
--
-- > primitives :: [Prim]
-- > primitives =
-- >   [ pr (0::Int)
-- >   , pr (1::Int)
-- >   , prim "+" ((+) :: Int -> Int -> Int)
-- >   , prim "*" ((*) :: Int -> Int -> Int)
-- >   , prim "-" ((-) :: Int -> Int -> Int)
-- >   ]
--
-- Step 3: call 'conjure' and see your generated function:
--
-- > factorial :: Int -> Int
-- > -- testing 3 combinations of argument values
-- > -- pruning with 27/65 rules
-- > -- looking through 3 candidates of size 1
-- > -- looking through 3 candidates of size 2
-- > -- looking through 9 candidates of size 3
-- > -- looking through 10 candidates of size 4
-- > -- looking through 32 candidates of size 5
-- > -- looking through 39 candidates of size 6
-- > -- looking through 185 candidates of size 7
-- > -- tested 107 candidates
-- > factorial 0  =  1
-- > factorial x  =  x * factorial (x - 1)
--
-- Factorial is discovered from scratch through a search.
-- We prune the search space using properties discovered
-- from the results of testing.
{-# LANGUAGE CPP #-}
module Conjure
  (
-- * Basic use
    conjure
  , Prim
  , pr
  , prim
  , prif
  , primOrdCaseFor

-- * Advanced use
  , conjureWithMaxSize
  , conjureWith
  , Args(..)
  , args

-- * Conjuring from a specification
  , conjureFromSpec
  , conjureFromSpecWith

-- * When using custom types
  , Conjurable (conjureExpress, conjureEquality, conjureTiers, conjureCases, conjureSubTypes, conjureSize)
  , Expr
  , val
  , value
  , reifyExpress
  , reifyEquality
  , reifyTiers
  , conjureType
  , Name (..)
  , Express (..)
  , deriveConjurable
  , deriveConjurableIfNeeded
  , deriveConjurableCascading

-- * Pure interfaces
  , Results (..)
  , conjpure
  , conjpureWith

-- * Helper test types
  , A, B, C, D, E, F
  )
where

import Conjure.Engine
import Conjure.Conjurable
import Conjure.Prim
import Conjure.Conjurable.Derive
