-- |
-- Module      : Conjure.Defn
-- Copyright   : (c) 2021 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- This module is part of 'Conjure'.
--
-- This module exports the 'Defn' type synonym and utilities involving it.
--
-- You are probably better off importing "Conjure".
{-# LANGUAGE TupleSections #-}
module Conjure.Defn
  ( Defn
  , Bndn
  , toDynamicWithDefn
  , devaluate
  , deval
  , devl
  , devalFast
  , showDefn
  , defnApparentlyTerminates
  , module Conjure.Expr
  )
where

import Conjure.Utils
import Conjure.Expr
import Data.Express
import Data.Express.Express
import Data.Express.Fixtures
import Data.Dynamic
import Control.Applicative ((<$>)) -- for older GHCs
import Test.LeanCheck.Utils ((-:>)) -- for toDynamicWithDefn

type Defn  =  [Bndn]
type Bndn  =  (Expr,Expr)

showDefn :: Defn -> String
showDefn  =  unlines . map show1
  where
  show1 (lhs,rhs)  =  showExpr lhs ++ "  =  " ++ showExpr rhs


-- | Evaluates an 'Expr' using the given 'Defn' as definition
--   when a recursive call is found.
toDynamicWithDefn :: (Expr -> Expr) -> Int -> Defn -> Expr -> Maybe Dynamic
toDynamicWithDefn exprExpr n cx  =  fmap (\(_,_,d) -> d) . re (n * sum (map (size . snd) cx)) n
  where
  (ef':_)  =  unfoldApp . fst $ head cx

  rev :: Typeable a => Int -> Int -> Expr -> Maybe (Int, Int, a)
  rev m n e  =  case re m n e of
                Nothing    -> Nothing
                Just (m,n,d) -> case fromDynamic d of
                                Nothing -> Nothing
                                Just x  -> Just (m, n, x)

  re :: Int -> Int -> Expr -> Maybe (Int, Int, Dynamic)
  re m n _  | n <= 0  =  error "toDynamicWithDefn: recursion limit reached"
  re m n _  | m <= 0  =  error "toDynamicWithDefn: evaluation limit reached"
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
    [] -> error "toDynamicWithDefn: empty application unfold"  -- should never happen
    [e] -> (m-1,n,) <$> toDynamic e
    (ef:exs) | ef == ef' -> headOr (error $ "toDynamicWithDefn: unhandled pattern " ++ show e)
                          [ re m (n-1) $ e' //- bs
                          | let e  =  foldApp (ef:map exprExpr exs)
                          , (a',e') <- cx
                          , Just bs <- [e `match` a']
                          ]
             | otherwise -> foldl ($$) (re m n ef) exs

  Just (m,n,d1) $$ e2  =  case re m n e2 of
                          Nothing -> Nothing
                          Just (m', n', d2) -> (m',n',) <$> dynApply d1 d2
  _ $$ _               =  Nothing

devaluate :: Typeable a => (Expr -> Expr) -> Int -> Defn -> Expr -> Maybe a
devaluate ee n fxpr e  =  toDynamicWithDefn ee n fxpr e >>= fromDynamic

deval :: Typeable a => (Expr -> Expr) -> Int -> Defn -> a -> Expr -> a
deval ee n fxpr x  =  fromMaybe x . devaluate ee n fxpr

devalFast :: Typeable a => (Expr -> Expr) -> Int -> Defn -> a -> Expr -> a
devalFast _ n [defn] x  =  reval defn n x

devl :: Typeable a => (Expr -> Expr) -> Int -> Defn -> Expr -> a
devl ee n fxpr  =  deval ee n fxpr (error "devl: incorrect type?")

defnApparentlyTerminates :: Defn -> Bool
defnApparentlyTerminates [(efxs, e)]  =  apparentlyTerminates efxs e
defnApparentlyTerminates _  =  True
