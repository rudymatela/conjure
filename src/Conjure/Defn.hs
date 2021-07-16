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
  ( Fxpr
  , fxprToDynamic
  , fevaluate
  , feval
  , fevl
  , deval
  , showFxpr
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
import Test.LeanCheck.Utils ((-:>)) -- for fxprToDynamic

type Fxpr  =  [(Expr,Expr)]


showFxpr :: Fxpr -> String
showFxpr  =  unlines . map show1
  where
  show1 (lhs,rhs)  =  showExpr lhs ++ "  =  " ++ showExpr rhs


-- | Evaluates an 'Expr' using the given 'Fxpr' as definition
--   when a recursive call is found.
fxprToDynamic :: (Expr -> Expr) -> Int -> Fxpr -> Expr -> Maybe Dynamic
fxprToDynamic exprExpr n cx  =  fmap (\(_,_,d) -> d) . re (n * sum (map (size . snd) cx)) n
  where
  (ef':_)  =  unfoldApp . fst $ head cx

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
                          , Just bs <- [e `match` a']
                          ]
             | otherwise -> foldl ($$) (re m n ef) exs

  Just (m,n,d1) $$ e2  =  case re m n e2 of
                          Nothing -> Nothing
                          Just (m', n', d2) -> (m',n',) <$> dynApply d1 d2
  _ $$ _               =  Nothing

fevaluate :: Typeable a => (Expr -> Expr) -> Int -> Fxpr -> Expr -> Maybe a
fevaluate ee n fxpr e  =  fxprToDynamic ee n fxpr e >>= fromDynamic

feval :: Typeable a => (Expr -> Expr) -> Int -> Fxpr -> a -> Expr -> a
feval ee n fxpr x  =  fromMaybe x . fevaluate ee n fxpr

deval :: Typeable a => (Expr -> Expr) -> Int -> Fxpr -> a -> Expr -> a
deval _ n [defn] x  =  reval defn n x

fevl :: Typeable a => (Expr -> Expr) -> Int -> Fxpr -> Expr -> a
fevl ee n fxpr  =  feval ee n fxpr (error "fevl: incorrect type?")

defnApparentlyTerminates :: Fxpr -> Bool
defnApparentlyTerminates [(efxs, e)]  =  apparentlyTerminates efxs e
defnApparentlyTerminates _  =  True
