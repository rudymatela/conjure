-- |
-- Module      : Conjure.Expr
-- Copyright   : (c) 2021 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- This internal module reexports 'Data.Express' along with a few other
-- utilities.
{-# LANGUAGE CPP #-}
module Conjure.Expr
  ( module Data.Express
  , module Data.Express.Fixtures

  , (>$$<)
  , funToVar
  , recursexpr
  , apparentlyTerminates
  , mayNotEvaluateArgument
  , applicationOld
  , compareSimplicity
  , ifFor
  , primitiveHoles
  , primitiveApplications

  , module Conjure.Utils
  )
where

import Conjure.Utils

import Data.Express
import Data.Express.Fixtures hiding ((-==-))

-- | /O(n)/.
-- Compares the simplicity of two 'Expr's.
-- An expression /e1/ is /strictly simpler/ than another expression /e2/
-- if the first of the following conditions to distingish between them is:
--
-- 1. /e1/ is smaller in size\/length than /e2/,
--    e.g.: @x + y < x + (y + z)@;
--
-- 2. or, /e1/ has less variable occurrences than /e2/,
--
-- 3. or, /e1/ has fewer distinct constants than /e2/,
--    e.g.: @1 + 1 < 0 + 1@.
--
-- They're otherwise considered of equal complexity,
-- e.g.: @x + y@ and @y + z@.
--
-- > > (xx -+- yy) `compareComplexity` (xx -+- (yy -+- zz))
-- > LT
--
-- > > (xx -+- yy) `compareComplexity` (xx -+- xx)
-- > EQ
--
-- > > (xx -+- xx) `compareComplexity` (one -+- xx)
-- > GT
--
-- > > (one -+- one) `compareComplexity` (zero -+- one)
-- > LT
--
-- > > (xx -+- yy) `compareComplexity` (yy -+- zz)
-- > EQ
--
-- > > (zero -+- one) `compareComplexity` (one -+- zero)
-- > EQ
compareSimplicity :: Expr -> Expr -> Ordering
compareSimplicity  =  (compare `on` length . values)
                   <> (compare `on` length . vars)
                   <> (compare `on` length . nubConsts)

-- | Makes the function in an application a variable
funToVar :: Expr -> Expr
funToVar (ef :$ ex)  =  funToVar ef :$ ex
funToVar ef@(Value nm _)  =  nm `varAsTypeOf` ef

-- | Expands recursive calls on an expression
--   until the given size limit is reached.
--
-- > > recursexpr 6 (ff xx) (ff xx)
-- > f x :: Int
--
-- > > recursexpr 6 (ff xx) (one -+- ff xx)
-- > 1 + (1 + (1 + (1 + f x))) :: Int
--
-- > > recursexpr 6 (ff xx) (if' pp one (xx -*- ff xx))
-- > (if p then 1 else x * (if p then 1 else x * f x)) :: Int
--
-- > > recursexpr 6 (ff xx) (if' pp one (xx -*- ff (gg xx)))
-- > (if p then 1 else x * (if p then 1 else g x * f (g (g x)))) :: Int
recursexpr :: Int -> Expr -> Expr -> Expr
recursexpr sz epat  =  re
  where
  err  =  error "recursexpr: pattern must contain an application of variables"
  (erf:vs)  =  unfoldApp epat
  re e' | not (all isVar (erf:vs))  =  err
        | e == e' || size e > sz    =  e
        | otherwise                 =  re e
    where
    e  =  re1 e'
    re1 e  =  case unfoldApp e of
              [e]                  -> e
              (ef:exs) | ef == erf -> e' //- zip vs exs
                       | otherwise -> foldApp (map re1 (ef:exs))

-- recursive call _only_ under an if
-- future-work: guess short-circuit operators

-- | Checks if the given recursive call apparently terminates.
--   The first argument indicates the functional variable indicating the
--   recursive call.
--
-- > > apparentlyTerminates ffE (ff xx)
-- > False
--
-- > > apparentlyTerminates ffE (if' pp zero (ff xx))
-- > True
--
-- This function only allows recursion in the else clause:
--
-- > > apparentlyTerminates ffE (if' pp (ff xx) zero)
-- > False
--
-- Of course, recursive calls as the condition are not allowed:
--
-- > > apparentlyTerminates ffE (if' (odd' (ff xx)) zero zero)
-- > False
apparentlyTerminates :: Expr -> Expr -> Bool
apparentlyTerminates eRecursiveCall  =  at
  where
  at (e1 :$ e2)  =  (mayNotEvaluateArgument e1 || at e2) && at e1
  at e  =  e /= eRecursiveCall

-- | Checks if the given functional expression may refrain from evaluating its
--   next argument.
--
--
-- > > mayNotEvaluateArgument (plus :$ xx)
-- > False
--
-- > > mayNotEvaluateArgument (andE :$ pp)
-- > True
--
-- This returns false for non-funcional value even if it involves an
-- application which may not evaluate its argument.
--
-- > > mayNotEvaluateArgument (andE :$ pp :$ qq)
-- > False
--
-- This currently works by checking if the function is an if, '&&' or '||'.
mayNotEvaluateArgument :: Expr -> Bool
mayNotEvaluateArgument (Value "if" ce :$ _ :$ _)  =  True
mayNotEvaluateArgument (Value "&&" ce :$ _)       =  True
mayNotEvaluateArgument (Value "||" ce :$ _)       =  True
mayNotEvaluateArgument _                          =  False

applicationOld :: Expr -> [Expr] -> Maybe Expr
applicationOld ff es  =  appn ff
  where
  appn ff
    | isFun ff   =  case [e | Just (_ :$ e) <- (map (ff $$)) es] of
                    [] -> Nothing  -- could not find type representative in es
                    (e:_) -> appn (ff :$ holeAsTypeOf e)
    | otherwise  =  Just ff

-- | Creates an if 'Expr' of the type of the given proxy.
--
-- > > ifFor (undefined :: Int)
-- > if :: Bool -> Int -> Int -> Int
--
-- > > ifFor (undefined :: String)
-- > if :: Bool -> [Char] -> [Char] -> [Char]
--
-- You need to provide this as part of your building blocks on the primitives
-- if you want recursive functions to be considered and produced.
ifFor :: Typeable a => a -> Expr
ifFor a  =  value "if" (\p x y -> if p then x else y `asTypeOf` a)

-- | Application cross-product between lists of Exprs
(>$$<) :: [Expr] -> [Expr] -> [Expr]
exs >$$< eys  =  catMaybes [ex $$ ey | ex <- exs, ey <- eys]

primitiveHoles :: [Expr] -> [Expr]
primitiveHoles prims  =  sort $ ph hs
  where
  hs  =  nub $ map holeAsTypeOf prims
  ph  =  iterateUntil (==) ps
  ps es  =  nub $ es ++ sq es
  sq es  =  nub $ map holeAsTypeOf $ es >$$< es
-- FIXME: the function above is quite inefficient.
--        Should run fast for a small number of types,
--        but if this number increases runtime may start
--        to become significant.

primitiveApplications :: [Expr] -> [[Expr]]
primitiveApplications prims  =  takeWhile (not . null)
                             $  iterate (>$$< primitiveHoles prims) prims
