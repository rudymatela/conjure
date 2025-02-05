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
  , candidateDeconstructionsFrom
  , candidateDeconstructionsFromHoled
  )
where

import Conjure.Defn
import Conjure.Conjurable
import Test.LeanCheck.Error (errorToFalse, errorToTrue)

-- | Returns whether an expression is a deconstruction
--   based on the results of testing.
--
-- This function takes three arguments:
--
-- 1. a (conjurable) function from where type info is obtained
-- 2. the maximum number of tests
-- 3. an 'Expr' of a possible deconstruction.
--
-- To facilitate use, this function is often used in curried form
-- in the contest of a "conjuring" function.  Given that the type
-- of the function-to-be-conjured is '[Int] -> Int' and we would
-- like to test for a maximum of 60 arguments, we would declare:
--
-- > > isDeconstruction = conjureIsDeconstruction (undefined :: [Int] -> Int) 60
--
-- Deconstructions are expressions that
-- decrease the size of all arguments
-- that have a size greater than 0.
-- Here are some examples:
--
-- > > import Data.Express.Fixtures
-- > > isDeconstruction  (minus :$ i_ :$ one)
-- > True
--
-- > > isDeconstruction (tail' is_)
-- > True
--
-- > > isDeconstruction (minus :$ i_ :$ two)
-- > True
--
-- > decandidates = [minus :$ i_ :$ one, tail' is_, head' is_, zero -*- i_]
-- > > filter isDeconstruction decandidates
-- > [ _ - 1 :: Int
-- > , tail _ :: [Int]
-- > ]
--
-- Well formed deconstructions should have exactly one typed hole:
--
-- > > isDeconstruction (i_ -+- i_)
-- > False
--
-- > > isDeconstruction (xx -+- one)
-- > False
--
-- We can only deconstruct to the same type.
-- Even though 'tail' is a deconstruction,
-- 'head' is not.
--
-- > > isDeconstruction (head' is_)
-- > False
--
-- Deconstructions should always reduce the size of expressions:
--
-- > > isDeconstruction (take' two is_)
-- > False
--
-- Lastly we disallow deconstructions that always map to values of size 0.
-- For the purposes of expression generation, in these cases
-- we are better of not recursing at all and returning a constant value!
--
-- > > isDeconstruction (zero -*- i_)
-- > False
conjureIsDeconstruction :: Conjurable f => f -> Int -> Expr -> Bool
conjureIsDeconstruction f maxTests ed
  =  length (holes ed) == 1  -- Well formed deconstruction, single hole.
  && typ h == typ ed         -- We can only deconstruct to the same type.
  && all is sizes            -- Do we always reduce size?
  && not (all iz sizes)      -- Disallow always mapping to values of size 0.
  where
  x << 0  =  True
  x << y  =  x < y
  is (sd,sx)  =  errorToFalse $ sd << sx
  iz (sd,sx)  =  errorToTrue $ sd == 0 || sx == 0
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
descends isDecOf efls efrs  =  any hasDeconstruction
                            $  classifyOn (typ . fst) (zip ls rs)
  where
  (_:ls)  =  unfoldApp efls
  (_:rs)  =  unfoldApp efrs

  hasDeconstruction :: [(Expr,Expr)] -> Bool
  hasDeconstruction lrs  =  or [b *<< bs | (b,bs) <- choices lrs]

  r << l  =  any (r `isDecOf`) (rvars l)
          || r `isStrictSubexprOf` l
          || isGround r && not (isGround l) && size r < size l

  r <<= l  =  r == l
           || r << l

  (l,r) *<< bs  =  r << l
                || or [ (l',r) *<<= bs'
                      | ((l',r'),bs') <- choices bs
                      , r' << l
                      ]

  (l,r) *<<= bs  =  r <<= l
                 || or [ (l',r) *<<= bs'
                       | ((l',r'),bs') <- choices bs
                       , r' <<= l
                       ]

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
