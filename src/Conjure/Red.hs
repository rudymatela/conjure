-- |
-- Module      : Conjure.Red
-- Copyright   : (c) 2021-2024 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- This module is part of "Conjure".
--
-- This defines functions that deal with recursive descent and deconstructions
module Conjure.Red
  ( conjureIsDeconstruction
  , descends
  , isDescent
  , candidateDeconstructionsFrom
  , candidateDeconstructionsFromHoled
  , argumentSubsets
  )
where

import Conjure.Defn
import Conjure.Conjurable
import Test.LeanCheck.Error (errorToFalse)

-- | Checks if an expression is a deconstruction.
--
-- There should be a single 'hole' in the expression.
--
-- It should decrease the size of all arguments that have
-- a size greater than 0.
conjureIsDeconstruction :: Conjurable f => f -> Int -> Expr -> Bool
conjureIsDeconstruction f maxTests ed
  =  length (holes ed) == 1  -- Well formed deconstruction, single hole.
  && typ h == typ ed         -- We can only deconstruct to the same type.
  && all is sizes            -- Do we always reduce size?
  && not (all iz sizes)      -- Disallow always mapping to values of size 0.
                             -- In this case, we are better off not recursing
                             -- and returning a constant value!
  where
  x << 0  =  True
  x << y  =  x < y
  is (sd,sx)  =  errorToFalse $ sd << sx
  iz (sd,sx)  =  errorToFalse $ sd == 0 || sx == 0
  -- We cannot simply conjureTiers for h here, because the deconstruction
  -- expression may contain variables, e.g.: @x `mod` _@.
  -- So conjureGrounds and the holeValue trick are required.
  sizes  =  map evalSize2 . take maxTests $ conjureGrounds f ed
  [h]  =  holes ed
  evalSize2 e  =  (evalSize e, evalSize $ holeValue e)
  evalSize e  =  eval (0::Int) (esize :$ e)
  esize  =  conjureSizeFor f h
  holeValue e  =  fromMaybe err . lookup h . fromMaybe err $ e `match` ed
  err  =  error "Conjure.conjureIsDeconstruction: the impossible happened"

-- | Returns whether a non-empty subset of arguments
--   descends arguments by deconstruction.
--
-- > > descends isDec (xxs -++- yys) (xxs -++- tail' yys)
-- > True
--
-- > > descends isDec (xxs -++- yys) (xxs -++- yys)
-- > False
--
-- > > descends isDec (xxs -++- yys) (head' xxs -:- tail xxs  -++-  head' yys -:- tail yys)
-- > False

-- > > descends isDec (xxs -\/- yys) (yys -\/- tail' xxs)
-- > True
--
-- The following are not so obvious:
--
-- > > descends isDec (xxs -++- yys) (tail' yys -++- yys)
-- > False
--
-- > > descends isDec (xxs -++- yys) (xx-:-xxs -++- tail' yys)
-- > True
--
-- For all possible sets of arguments (2^n - 1 elements: 1 3 7 15 31),
-- see if any projects the same variables while only using deconstructions
-- and where there is at least a single deconstruction.
descends :: (Expr -> Expr -> Bool) -> Expr -> Expr -> Bool
descends  =  descends2
-- TODO: test the above with gcd and other interesting cases
-- TODO: migrate back to descends1 after fixing argumentSubsets issues

descends2 :: (Expr -> Expr -> Bool) -> Expr -> Expr -> Bool
descends2 isDecOf e' e  =  any (uncurry $ isDescent isDecOf) $ map unzip ss
  where
  ss  =  init $ sets exys
  exys  =  zip exs eys
  (_:exs)  =  unfoldApp e'
  (_:eys)  =  unfoldApp e

-- the old descends
descends1 :: (Expr -> Expr -> Bool) -> Expr -> Expr -> Bool
descends1 isDecOf efxs  =  any desc . argumentSubsets efxs
  where
  desc exys  =  all (isNotConstruction isDecOf) exys
             && any (isDeconstruction isDecOf) exys

-- > > isDescent [xx -:- xxs, yys]  [yys, xxs]
-- > True
--
-- > > isDescent [xx -:- xxs, yys]  [yys, xx -:- xxs]
-- > False
isDescent :: (Expr -> Expr -> Bool) -> [Expr] -> [Expr] -> Bool
isDescent isDecOf ps es  =  any is es
                         && all iz es
  where
  is e | isVar e    =  any (\p -> 1 {-size e-} < size p && e `elem` vars p) ps
       | otherwise  =  any (e `isDecOf`) $ concatMap vars ps
  iz e | isVar e    =  True
       | otherwise  =  all (\p -> e `isDecOf` p || size e <= size p) ps
-- TODO: closely review isDescent and descends
-- TODO: after sets, filter sets of values of the same type
-- TODO: improve "iz" a.k.a. isNotConstruction
-- TODO: create test/engine.hs and thoroughly test descends and isDescent


isDeconstruction :: (Expr -> Expr -> Bool) -> (Expr,Expr) -> Bool
isDeconstruction isDecOf (el,er)
  | isVar el   =  er `isDecOf` el
  | otherwise  =  size er < size el

isNotConstruction :: (Expr -> Expr -> Bool) -> (Expr,Expr) -> Bool
isNotConstruction isDecOf (el,er)
  | isVar el   =  er == el || er `isDecOf` el
  | otherwise  =  size er <= size el


-- | Lists pairs of argument subsets for the two given 'Expr's.
--
-- The listed sets are not empty and must have values of the same type.
argumentSubsets :: Expr -> Expr -> [[(Expr,Expr)]]
argumentSubsets efxs efys  =
  [ zip els ers
  | aas <- sets $ zip exs eys
  , not (null aas)
  , allEqualOn (typ . fst) aas
  , length aas == 1 || none (\(el,er) -> rvars el == rvars er) aas
  , let (els, ers) = both (sortOn rvars) $ unzip aas
  , map rvars els == map rvars ers
  ]
  where
  (_:exs)  =  unfoldApp efxs
  (_:eys)  =  unfoldApp efys
-- TODO: allow mapping involving multiple variables (such as gcd)
-- TODO: allow mapping arguments to constant values
-- TODO: try to do maximum pairing in each set
-- TODO: sort by smallest valid set and discard sets that contain it

-- | Compute candidate deconstructions from an 'Expr'.
--
-- This is used in the implementation of 'Conjure.Engine.candidateDefnsC'
-- followed by 'conjureIsDeconstruction'.
--
-- > > candidateDeconstructionsFrom (xx `mod'` yy)
-- > [ _ `mod` y
-- > , x `mod` _
-- > ]
--
-- To be constrasted with 'candidateDeconstructionsFromHoled'.
candidateDeconstructionsFrom :: Expr -> [Expr]
candidateDeconstructionsFrom e  =
  [ e'
  | v <- vars e
  , typ v == typ e
  , let e' = e //- [(v, holeAsTypeOf v)]
  , length (holes e') == 1
  ]

-- | Compute candidate deconstructions from an 'Expr'.
--
-- This is used in the implementation of 'Conjure.Engine.candidateExprs'
-- followed by 'conjureIsDeconstruction'.
--
-- This is similar to 'canonicalVariations'
-- but always leaves a hole
-- of the same return type as the given expression.
--
-- > > candidateDeconstructionsFrom (i_ `mod'` i_)
-- > [ _ `mod` x
-- > , x `mod` _
-- > ]
--
-- To be contrasted with 'candidateDeconstructionsFrom'
candidateDeconstructionsFromHoled :: Expr -> [Expr]
candidateDeconstructionsFromHoled e  =  map (//- [(v, h)])
                                     $  concatMap canonicalVariations
                                     $  deholings v e
  where
  h  =  holeAsTypeOf e
  v  =  "_#_" `varAsTypeOf` e  -- a marker variable with an invalid name
  -- at some point I should get rid of candidateDeconstructionsFrom in favour
  -- of this one
