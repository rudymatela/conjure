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
-- > ingredients :: [Ingredient]
-- > ingredients =
-- >   [ unfun (0::Int)
-- >   , unfun (1::Int)
-- >   , fun "+" ((+) :: Int -> Int -> Int)
-- >   , fun "*" ((*) :: Int -> Int -> Int)
-- >   , fun "-" ((-) :: Int -> Int -> Int)
-- >   ]
--
-- Step 3: call 'conjure' and see your generated function:
--
-- > > conjure "factorial" factorial ingredients
-- > factorial :: Int -> Int
-- > -- 0.1s, testing 4 combinations of argument values
-- > -- 0.8s, pruning with 27/65 rules
-- > -- 0.8s, 3 candidates of size 1
-- > -- 0.9s, 3 candidates of size 2
-- > -- 0.9s, 7 candidates of size 3
-- > -- 0.9s, 8 candidates of size 4
-- > -- 0.9s, 28 candidates of size 5
-- > -- 0.9s, 35 candidates of size 6
-- > -- 0.9s, 167 candidates of size 7
-- > -- 0.9s, tested 95 candidates
-- > factorial 0  =  1
-- > factorial x  =  x * factorial (x - 1)
--
-- The above example takes less than a second to run in a modern laptop.
--
-- Factorial is discovered from scratch through a search.
-- We prune the search space using properties discovered
-- from the results of testing.
--
-- Conjure is not limited to integers,
-- it works for functions over algebraic data types too.
-- See:
--
-- > take' :: Int -> [a] -> [a]
-- > take' 0 [x]    =  []
-- > take' 1 [x]    =  [x]
-- > take' 0 [x,y]  =  []
-- > take' 1 [x,y]  =  [x]
-- > take' 2 [x,y]  =  [x,y]
-- > take' 3 [x,y]  =  [x,y]
--
-- > > conjure "take" (take' :: Int -> [A] -> [A])
-- > >   [ unfun (0 :: Int)
-- > >   , unfun (1 :: Int)
-- > >   , unfun ([] :: [A])
-- > >   , fun ":" ((:) :: A -> [A] -> [A])
-- > >   , fun "-" ((-) :: Int -> Int -> Int)
-- > >   ]
-- > take :: Int -> [A] -> [A]
-- > -- testing 153 combinations of argument values
-- > -- pruning with 4/7 rules
-- > -- ...  ...  ...  ...  ...  ...
-- > -- 0.4s, 6 candidates of size 8
-- > -- 0.4s, 5 candidates of size 9
-- > -- 0.4s, tested 15 candidates
-- > take 0 xs  =  []
-- > take x []  =  []
-- > take x (y:xs)  =  y:take (x - 1) xs
--
-- The above example also takes less than a second to run in a modern laptop.
-- The selection of functions in the list of ingredients was minimized
-- to what was absolutely needed here.
-- With a larger collection as ingredients YMMV.
--
-- Conjure works for user-defined algebraic data types too,
-- given that they are made instances of the 'Conjurable' typeclass.
-- For types without data invariants,
-- it should be enough to call 'deriveConjurable'
-- to create an instance using TH.
{-# LANGUAGE CPP #-}
module Conjure
  (
-- * Basic use
    conjure
  , Ingredient
  , fun
  , unfun
  , guard
  , iif
  , ordcase

-- * Basic configuration parameters
  , maxTests
  , target
  , maxSize

-- * Conjuring from a specification
  , conjureFromSpec

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

-- * Helper test types
  , A, B, C, D, E, F

-- * Advanced options
  , maxRecursions
  , maxEquationSize
  , maxSearchTests
  , maxDeconstructionSize
  , maxConstantSize
  , maxPatternSize
  , maxPatternDepth

-- * Debug options
  , showCandidates
  , showTheory
  , singlePattern
  , showTests
  , showPatterns
  , showDeconstructions
  , carryOn

-- * Advanced pruning options
  , dontRewrite
  , dontRequireDescent
  , omitAssortedPruning
  , omitEarlyTests
  , dontCopyBindings
  , nonAtomicNumbers
  , uniqueCandidates

-- * Deprecated functions
  , Prim
  , pr
  , prim
  , con
  )
where

import Conjure.Engine
import Conjure.Conjurable
import Conjure.Ingredient
import Conjure.Conjurable.Derive
import Conjure.Settings
