-- |
-- Module      : TEst.Candidates
-- Copyright   : (c) 2021 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- Reference implementations of candidate generation.
--
-- Bottom-up and top-down
module Test.Candidates
  ( boupCandidates
  , townCandidates
  , dropTrail
  )
where

import Conjure.Expr
import Test.LeanCheck

boupCandidates :: Expr -> [Expr] -> [[Expr]]
boupCandidates appn primitives  =  dropTrail $ filterT (\e -> typ e == typ appn) boup
  where
  boup :: [[Expr]]
  boup = expressionsT [primitives]

  expressionsT ds  =  ds \/ (delay $ productMaybeWith ($$) es es)
    where
    es = expressionsT ds

townCandidates :: Expr -> [Expr] -> [[Expr]]
townCandidates appn primitives  =  normalizeT
                                $  filterT (not . hasHole)
                                $  town [[holeAsTypeOf appn]]
  where
  town :: [[Expr]] -> [[Expr]]
  town ((e:es):ess)  =  [[e]] \/ town (expand e \/ (es:ess))
  town ([]:ess)  =  []:town ess
  town []  =  []

  expand :: Expr -> [[Expr]]
  expand e  =  case holesBFS e of
    [] -> []
    (h:_) -> mapT (fillBFS e) (replacementsFor h)

  replacementsFor :: Expr -> [[Expr]]
  replacementsFor h  =  filterT (\e -> typ e == typ h)
                     $  primitiveApplications primitives

-- like normalizeT, but considers 6 empty tiers as an infinite trail of tiers
-- this should only be used on testing
dropTrail :: [[a]] -> [[a]]
dropTrail []  =  []
dropTrail [[]]  =  []
dropTrail [[],[]]  =  []
dropTrail [[],[],[]]  =  []
dropTrail [[],[],[],[]]  =  []
dropTrail [[],[],[],[],[]]  =  []
dropTrail [[],[],[],[],[],[]]  =  []
dropTrail ([]:[]:[]:[]:[]:[]:_)  =   []
dropTrail (xs:xss)  =  xs:dropTrail xss
