-- |
-- Module      : Conjure.Defn.Redundancy
-- Copyright   : (c) 2021-2025 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- This module is part of "Conjure".
--
-- This module exports functions that check redundancy in 'Defn's.
--
-- You are probably better off importing "Conjure".
module Conjure.Defn.Redundancy
  ( isRedundantDefn
  , isRedundantBySubsumption
  , isRedundantByRepetition
  , isRedundantByIntroduction
  , isRedundantModuloRewriting
  , hasRedundantRecursion
  , subsumedWith
  , simplifyDefn
  , introduceVariableAt
  )
where

import Conjure.Defn

-- | Returns whether the given 'Defn' is redundant
--   with regards to repetitions on RHSs.
--
-- Here is an example of a redundant 'Defn':
--
-- > 0 ? 0  =  1
-- > 0 ? x  =  1
-- > x ? 0  =  x
-- > x ? y  =  x
--
-- It is redundant because it is equivalent to:
--
-- > 0 ? _  =  1
-- > x ? _  =  x
--
-- This function safely handles holes on the RHSs
-- by being conservative in cases these are found:
-- nothing can be said before their fillings.
isRedundantDefn :: Defn -> Bool
isRedundantDefn d  =  isRedundantBySubsumption d
                   || isRedundantByRepetition d
                   || isRedundantByRootRecursions d
--                 || isRedundantByIntroduction d
-- we do not use isRedundantByIntroduction above
-- as it does not pay off in terms of runtime vs number of pruned candidates
--
-- The number of candidates is reduced usually by less than 1%
-- and the runtime increases by 50% or sometimes 100%.


-- | Returns whether the given 'Defn' is redundant
--   with regards to repetitions on RHSs.
--
-- Here is an example of a redundant 'Defn':
--
-- > 0 ? 0  =  1
-- > 0 ? x  =  1
-- > x ? 0  =  x
-- > x ? y  =  x
--
-- It is redundant because it is equivalent to:
--
-- > 0 ? _  =  1
-- > x ? _  =  x
--
-- @1@ and @x@ are repeated in the results for when
-- the first arguments are @0@ and @x@.
isRedundantByRepetition :: Defn -> Bool
isRedundantByRepetition d  =  any anyAllEqual shovels
  where
  nArgs  =  length . tail . unfoldApp . fst $ head d
  shovels :: [Expr -> Expr]
  shovels  =  [digApp n | n <- [1..nArgs]]
  anyAllEqual :: (Expr -> Expr) -> Bool
  anyAllEqual shovel  =  any (\bs -> allEqual2 bs && isDefined bs)
                      .  classifyOn fst
                      .  map (canonicalizeBndn . first shovel)
                      $  d

-- | Returns whether the given 'Defn' is redundant
--   with regards to case elimination
--
-- The following is redundant according to this criterium:
--
-- > foo []  =  []
-- > foo (x:xs)  =  x:xs
--
-- It is equivalent to:
--
-- > foo xs = xs
--
-- The following is also redundant:
--
-- > [] ?? xs  =  []
-- > (x:xs) ?? ys  =  x:xs
--
-- as it is equivalent to:
--
-- > xs ?? ys == xs
--
-- This function is not used as one of the criteria in 'isRedundantDefn'
-- because it does not pay-off
-- in terms of runtime vs number of pruned candidates.
isRedundantByIntroduction :: Defn -> Bool
isRedundantByIntroduction d  =  any anyAllEqual [1..nArgs]
  where
  nArgs  =  length . tail . unfoldApp . fst $ head d
  anyAllEqual :: Int -> Bool
  anyAllEqual i  =  any (\bs -> length bs >= 2 && isDefined bs && noConflicts i bs)
                 .  classifyOn (digApp i . fst)
                 .  map (canonicalizeBndnLast i)
                 $  d
  noConflicts :: Int -> [Bndn] -> Bool
  noConflicts i bs  =  case listConflicts (map snd bs) of
                       [] -> True
                       [es] -> es == [efxs $!! i | (efxs,_) <- bs]
                       _ -> False

-- | Returns whether the given 'Defn' is redundant
--   with regards to recursions
--
-- The following is redundant:
--
-- > xs ?? []  =  []
-- > xs ?? (x:ys)  =  xs ?? []
--
-- The LHS of a base-case pattern, matches the RHS of a recursive pattern.
-- The second RHS may be replaced by simply @[]@ which makes it redundant.
hasRedundantRecursion :: Defn -> Bool
hasRedundantRecursion d  =  not (null rs) && any matchesRHS bs
  where
  (bs,rs)  =  partition isBaseCase d
  matchesRHS (lhs,_)  =  any ((`hasAppInstanceOf` lhs) . snd) rs

-- | Returns whether a given 'Defn' is redundant
--   with regards to root recursions.
--
-- When there is a single constant base case and all recursive calls
-- are at the root: we have a redundant function.
-- (Modulo error functions, which are undesired anyway.)
--
-- Here is an example:
--
-- > xs ? []  =  []
-- > xs ? (x:ys)  =  xs ? ys
--
-- Above it does not really pays off to follow the recursive calls,
-- at the end we always reach an empty list.
--
-- Here is a more complex example:
--
-- > [] ? xs  =  []
-- > (x:xs) ? []  =  []
-- > (x:xs) ? (y:ys)  =  xs ? ys
--
-- We are bound to reach [] when following the recursion.
isRedundantByRootRecursions :: Defn -> Bool
isRedundantByRootRecursions d  =  case partition isGround $ map snd d of
  ([b], rs@(_:_))  ->  all isHole rs
  (bs@(_:_), rs@(_:_))  -> all isHole rs && all isGround bs && allEqual bs
  _  -> False

-- | Introduces a hole at a given position in the binding:
--
-- > > introduceVariableAt 1 (xxs -?- (yy -:- yys), (yy -:- yys) -++- (yy -:- yys))
-- > (xs ? (y:ys) :: [Int],(y:ys) ++ (y:ys) :: [Int])
--
-- > > introduceVariableAt 2 (xxs -?- (yy -:- yys), (yy -:- yys) -++- (yy -:- yys))
-- > (xs ? x :: [Int],x ++ x :: [Int])
--
-- Relevant occurrences are replaced.
introduceVariableAt :: Int -> Bndn -> Bndn
introduceVariableAt i b@(l,r)
  | isVar p    =  b -- already a variable
-- | isGround p  =  (newVar, r)  -- enabling catches a different set of candidates
  | otherwise  =  unfoldPair
               $  foldPair b // [(p,newVar)]
  where
  p  =  l $!! i
  newVar  =  newName `varAsTypeOf` p
  newName  =  head $ variableNamesFromTemplate "x" \\ varnames l
  varnames :: Expr -> [String]
  varnames e  =  [n | Value ('_':n) _ <- vars e]

-- | Returns whether the given 'Defn' is redundant
--   with regards to subsumption by latter patterns
--
-- Here is an example of a redundant 'Defn' by this criterium:
--
-- > foo 0  =  0
-- > foo x  =  x
isRedundantBySubsumption :: Defn -> Bool
isRedundantBySubsumption  =  is . map foldPair . filter isCompleteBndn
  -- above, we could have used noUnbound instead of isCompleteBndn
  -- we use isCompleteBndn as it is faster
  where
  is []  =  False
  is (b:bs)  =  any (b `isInstanceOf`) bs || is bs

-- | Returns whether the given 'Defn' is redundant modulo
--   subsumption and rewriting.
--
-- (cf. 'subsumedWith')
isRedundantModuloRewriting :: (Expr -> Expr) -> Defn -> Bool
isRedundantModuloRewriting normalize  =  is
  where
  is []  =  False
  is (b:bs)  =  any (subsumedWith normalize b) bs
             || is bs

-- | Simplifies a definition by removing redundant patterns
--
-- This may be useful in the following case:
--
-- > 0 ^^^ 0  =  0
-- > 0 ^^^ x  =  x
-- > x ^^^ 0  =  x
-- > _ ^^^ _  =  0
--
-- The first pattern is subsumed by the last pattern.
simplifyDefn :: Defn -> Defn
simplifyDefn []  =  []
simplifyDefn (b:bs)  =  [b | none (foldPair b `isInstanceOf`) $ map foldPair bs]
                     ++ simplifyDefn bs

-- | Returns whether a binding is subsumed by another modulo rewriting
--
-- > > let normalize = (// [(zero -+- zero, zero)])
-- > > subsumedWith normalize (ff zero, zero) (ff xx, xx -+- xx)
-- > True
--
-- > > subsumedWith normalize (ff zero, zero) (ff xx, xx -+- one)
-- > False
--
-- > > subsumedWith normalize (zero -?- xx, zero) (xx -?- yy, xx -+- xx)
-- > True
--
-- (cf. 'isRedundantModuloRewriting')
subsumedWith :: (Expr -> Expr) -> Bndn -> Bndn -> Bool
subsumedWith normalize (lhs1,rhs1) (lhs2,rhs2)
  | hasHole rhs2  =  False
  | otherwise  =  case lhs1 `match` lhs2 of
                  Nothing -> False
                  Just bs -> snd (canonicalizeBndn (lhs2, normalize $ rhs2 //- bs))
                          == snd (canonicalizeBndn (lhs1, rhs1))
