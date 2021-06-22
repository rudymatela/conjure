-- |
-- Module      : Conjure.Cases
-- Copyright   : (c) 2021 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- This module is part of 'Conjure'.
--
-- This module defines the 'Cases' typeclass
-- that allows listing cases of a type
-- encoded as 'Expr's.
--
-- You are probably better off importing "Conjure".
{-# LANGUAGE TupleSections #-}
module Conjure.Cases
  ( Cases (..)
  , Fxpr
  , fxprToDynamic
  , fevaluate
  , feval
  )
where

import Conjure.Utils
import Data.Express
import Data.Express.Express
import Data.Express.Fixtures
import Data.Dynamic
import Control.Applicative ((<$>)) -- for older GHCs
import Test.LeanCheck.Utils ((-:>)) -- for fxprToDynamic

type Fxpr  =  (Expr, Cxpr)
type Cxpr  =  [([Expr],Expr)]
-- consider changing back to [(Expr,Expr)] as it will be easier to match
-- I wonder if I can encode a Cxpr into a simple Expr with some cleverness...


-- | Evaluates an 'Expr' using the given 'Fxpr' as definition
--   when a recursive call is found.
fxprToDynamic :: (Expr -> Expr) -> Int -> Fxpr -> Expr -> Maybe Dynamic
fxprToDynamic exprExpr n (ef',cx)  =  fmap (\(_,_,d) -> d) . re (n * {- FIXME: -} 12) n
  where

  rev :: Typeable a => Int -> Int -> Expr -> Maybe (Int, Int, a)
  rev m n e  =  case re m n e of
                Nothing    -> Nothing
                Just (m,n,d) -> case fromDynamic d of
                                Nothing -> Nothing
                                Just x  -> Just (m, n, x)

  re :: Int -> Int -> Expr -> Maybe (Int, Int, Dynamic)
  re m n _  | n <= 0  =  error "fxprToDynamic: recursion limit reached"
  re m n _  | m <= 0  =  error "fxprToDynamic: evaluation limit reached"
  re m n (Value "if" _ :$ ec :$ ex :$ ey)  =  case rev m n ec of
    Nothing    -> Nothing
    Just (m,n,True)  -> re m n ex
    Just (m,n,False) -> re m n ey
  re m n (Value "||" _ :$ ep :$ eq)  =  case rev m n ep of
    Nothing        -> Nothing
    Just (m,n,True)  -> (m,n,) <$> toDynamic (val True)
    Just (m,n,False) -> re m n eq
  re m n (Value "&&" _ :$ ep :$ eq)  =  case rev m n ep of
    Nothing    -> Nothing
    Just (m,n,True)  -> re m n eq
    Just (m,n,False) -> (m,n,) <$> toDynamic (val False)
  re m n e  =  case unfoldApp e of
    [] -> error "fxprToDynamic: empty application unfold"  -- should never happen
    [e] -> (m-1,n,) <$> toDynamic e
    (ef:exs) | ef == ef' -> headOr (error $ "fxprToDynamic: unhandled pattern " ++ show e)
                          [ re m (n-1) $ e' //- bs
                          | let e  =  foldApp (ef:map exprExpr exs)
                          , (a',e') <- cx
                          , Just bs <- [e `match` foldApp (ef:a')]
                          ]
             | otherwise -> foldl ($$) (re m n ef) exs

  Just (m,n,d1) $$ e2  =  case re m n e2 of
                          Nothing -> Nothing
                          Just (m', n', d2) -> (m',n',) <$> dynApply d1 d2
  _ $$ _               =  Nothing
-- provisional example:
-- > > fromDynamic <$> fxprToDynamic (exprExprFor (undefined :: [Int])) 6 sumFxpr (var "sum" (undefined :: [Int] -> Int) :$ val [1,2,3,11::Int]) :: Maybe (Maybe Int)
-- > Just (Just 17)

fevaluate :: Typeable a => (Expr -> Expr) -> Int -> Fxpr -> Expr -> Maybe a
fevaluate ee n fxpr e  =  fxprToDynamic ee n fxpr e >>= fromDynamic

feval :: Typeable a => (Expr -> Expr) -> Int -> Fxpr -> a -> Expr -> a
feval ee n fxpr x  =  fromMaybe x . fevaluate ee n fxpr

class Express a => Cases a where
  cases :: a -> [Expr]
  cases _  =  []

-- Atomic types have no cases.
-- It's out of the scope of this typeclass to handle these.
instance Cases Int
instance Cases Integer
instance Cases Char

instance Cases () where
  cases _  =  [val ()]

instance Cases Bool where
  cases _  =  [val False, val True]

instance Express a => Cases [a] where
  cases xs  =  [ val ([] -: xs)
               , value ":" ((:) ->>: xs) :$ hole x :$ hole xs
               ]
    where
    x  =  head xs

instance (Express a, Express b) => Cases (a,b) where
  cases xy  =  [value "," ((,) ->>: xy) :$ hole x :$ hole y]
    where
    (x,y) = (undefined,undefined) -: xy

instance (Express a, Express b, Express c) => Cases (a,b,c) where
  cases xyz  =  [value ",," ((,,) ->>>: xyz) :$ hole x :$ hole y :$ hole z]
    where
    (x,y,z) = (undefined,undefined,undefined) -: xyz

instance Express a => Cases (Maybe a) where
  cases mx  =  [ value "Nothing" (Nothing -: mx)
               , value "Just" (Just ->: mx) :$ hole x
               ]
    where
    x  =  Just undefined -: mx


instance (Express a, Express b) => Cases (Either a b) where
  cases exy  =  [ value "Left" (Left ->: exy) :$ hole x
                , value "Right" (Right ->: exy) :$ hole y
                ]
    where
    x  =  Left undefined -: exy
    y  =  Right undefined -: exy
