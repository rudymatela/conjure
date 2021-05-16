-- |
-- Module      : Conjure.Candidates
-- Copyright   : (c) 2021 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- Reference implementations of candidate generation.
--
-- Bottom-up and top-down
module Conjure.Candidates
  ( boupCandidates
  , townCandidates
  )
where

import Conjure.Expr
import Test.LeanCheck

boupCandidates :: Expr -> [Expr] -> [[Expr]]
boupCandidates appn primitives  =  undefined

townCandidates :: Expr -> [Expr] -> [[Expr]]
townCandidates appn primitives  =  town [[holeAsTypeOf appn]]
  where
  town :: [[Expr]] -> [[Expr]]
  town ((e:es):ess)  =  [[e]] \/ town (expand e \/ (es:ess))
  town ([]:ess)  =  []:town ess
  town []  =  []

  papps :: [[Expr]]
  papps  =  primitiveApplications primitives

  pappsFor :: Expr -> [[Expr]]
  pappsFor h  =  filterT (\e -> typ e == typ h) papps

  expand :: Expr -> [[Expr]]
  expand e  =  case holesBFS e of
    [] -> []
    (h:_) -> mapT (fillBFS e) (pappsFor h)

  expressionsT :: [[Expr]] -> [[Expr]]
  expressionsT ds  =  ds \/ (delay $ productMaybeWith ($$) es es)
    where
    es = expressionsT ds
