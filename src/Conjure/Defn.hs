-- |
-- Module      : Conjure.Defn
-- Copyright   : (c) 2021 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- This module is part of "Conjure".
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
  , printDefn
  , defnApparentlyTerminates
  , isRedundantDefn
  , isRedundantBySubsumption
  , isRedundantByRepetition
  , isRedundantByIntroduction
  , isCompleteDefn
  , isCompleteBndn
  , simplifyDefn
  , canonicalizeBndn
  , hasUnbound
  , noUnbound
  , isUndefined
  , isDefined
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
import Test.LeanCheck.Utils ((-:>), classifyOn)

-- | A function definition as a list of top-level case bindings ('Bndn').
--
-- Here is an example using the notation from "Data.Express.Fixtures":
--
-- > sumV :: Expr
-- > sumV  =  var "sum" (undefined :: [Int] -> Int)
-- >
-- > (=-) = (,)
-- > infixr 0 =-
-- >
-- > sumDefn :: Defn
-- > sumDefn  =  [ sum' nil           =-  zero
-- >             , sum' (xx -:- xxs)  =-  xx -+- (sumV :$ xxs)
-- >             ]  where  sum' e  =  sumV :$ e
type Defn  =  [Bndn]

-- | A single binding in a definition ('Defn').
type Bndn  =  (Expr,Expr)

-- | Pretty-prints a 'Defn' as a 'String':
--
-- > > putStr $ showDefn sumDefn
-- > sum []  =  0
-- > sum (x:xs)  =  x + sum xs
showDefn :: Defn -> String
showDefn  =  unlines . map show1
  where
  show1 (lhs,Value "if" _ :$ c :$ t :$ e)  =  lhseqs ++ "if " ++ showExpr c
                                   ++ "\n" ++ spaces ++ "then " ++ showExpr t
                                   ++ "\n" ++ spaces ++ "else " ++ showExpr e
                                              where
                                              lhseqs  =  showExpr lhs ++ "  =  "
                                              spaces  =  map (const ' ') lhseqs
  show1 (lhs,rhs)  =  showExpr lhs ++ "  =  " ++ showExpr rhs

-- | Pretty-prints a 'Defn' to the screen.
--
-- > > printDefn sumDefn
-- > sum []  =  0
-- > sum (x:xs)  =  x + sum xs
printDefn :: Defn -> IO ()
printDefn  =  putStr . showDefn

type Memo  =  [(Expr, Maybe Dynamic)]

-- | Evaluates an 'Expr' to a 'Dynamic' value
--   using the given 'Defn' as definition
--   when a recursive call is found.
--
-- Arguments:
--
-- 1. a function that deeply reencodes an expression (cf. 'expr')
-- 2. the maximum number of recursive evaluations
-- 3. a 'Defn' to be used when evaluating the given 'Expr'
-- 4. an 'Expr' to be evaluated
--
-- This function cannot be used to evaluate a functional value for the given 'Defn'
-- and can only be used when occurrences of the given 'Defn' are fully applied.
--
-- The function the deeply reencodes an 'Expr' can be defined using
-- functionality present in "Conjure.Conjurable".  Here's a quick-and-dirty version
-- that is able to reencode 'Bool's, 'Int's and their lists:
--
-- > exprExpr :: Expr -> Expr
-- > exprExpr  =  conjureExpress (undefined :: Bool -> [Bool] -> Int -> [Int] -> ())
--
-- The maximum number of recursive evaluations counts in two ways:
--
-- 1. the maximum number of entries in the recursive-evaluation memo table;
-- 2. the maximum number of terminal values considered (but in this case the
--    limit is multiplied by the _size_ of the given 'Defn'.
--
-- These could be divided into two separate parameters but
-- then there would be an extra _dial_ to care about...
--
-- (cf. 'devaluate', 'deval', 'devl')
toDynamicWithDefn :: (Expr -> Expr) -> Int -> Defn -> Expr -> Maybe Dynamic
toDynamicWithDefn exprExpr mx cx  =  fmap (\(_,_,d) -> d) . re (mx * sum (map (size . snd) cx)) []
  where
  (ef':_)  =  unfoldApp . fst $ head cx

  -- recursively evaluate an expression, the entry point
  re :: Int -> Memo -> Expr -> Maybe (Int, Memo, Dynamic)
  re n m _  | length m > mx  =  error "toDynamicWithDefn: recursion limit reached"
  re n m _  | n <= 0  =  error "toDynamicWithDefn: evaluation limit reached"
  re n m (Value "if" _ :$ ec :$ ex :$ ey)  =  case rev n m ec of
    Nothing    -> Nothing
    Just (n,m,True)  -> re n m ex
    Just (n,m,False) -> re n m ey
  re n m (Value "||" _ :$ ep :$ eq)  =  case rev n m ep of
    Nothing        -> Nothing
    Just (n,m,True)  -> (n,m,) <$> toDynamic (val True)
    Just (n,m,False) -> re n m eq
  re n m (Value "&&" _ :$ ep :$ eq)  =  case rev n m ep of
    Nothing    -> Nothing
    Just (n,m,True)  -> re n m eq
    Just (n,m,False) -> (n,m,) <$> toDynamic (val False)
  re n m e  =  case unfoldApp e of
    [] -> error "toDynamicWithDefn: empty application unfold"  -- should never happen
    [e] -> (n-1,m,) <$> toDynamic e
    (ef:exs) | ef == ef' -> red n m (foldApp (ef:map exprExpr exs))
             | otherwise -> foldl ($$) (re n m ef) exs

  -- like 're' but is bound to an actual Haskell value instead of a Dynamic
  rev :: Typeable a => Int -> Memo -> Expr -> Maybe (Int, Memo, a)
  rev n m e  =  case re n m e of
                Nothing    -> Nothing
                Just (n,m,d) -> case fromDynamic d of
                                Nothing -> Nothing
                                Just x  -> Just (n, m, x)

  -- evaluates by matching on one of cases of the actual definition
  -- should only be used to evaluate an expr of the form:
  -- ef' :$ exprExpr ex :$ exprExpr ey :$ ...
  red :: Int -> Memo -> Expr -> Maybe (Int, Memo, Dynamic)
  red n m e  |  size e > n  =  error "toDynamicWithDefn: argument-size limit reached"
  red n m e  =  case lookup e m of
    Just Nothing -> error $ "toDynamicWithDefn: loop detected " ++ show e
    Just (Just d) -> Just (n,m,d)
    Nothing -> case [re n ((e,Nothing):m) $ e' //- bs | (a',e') <- cx, Just bs <- [e `match` a']] of
               [] -> error $ "toDynamicWithDefn: unhandled pattern " ++ show e
               (Nothing:_) -> Nothing
               (Just (n,m,d):_) -> Just (n,[(e',if e == e' then Just d else md) | (e',md) <- m],d)

  ($$) :: Maybe (Int,Memo,Dynamic) -> Expr -> Maybe (Int, Memo, Dynamic)
  Just (n,m,d1) $$ e2  =  case re n m e2 of
                          Nothing -> Nothing
                          Just (n', m', d2) -> (n',m',) <$> dynApply d1 d2
  _ $$ _               =  Nothing

-- | Evaluates an 'Expr' expression into 'Just' a regular Haskell value
--   using a 'Defn' definition when it is found.
--   If there's a type-mismatch, this function returns 'Nothing'.
--
-- This function requires a 'Expr'-deep-reencoding function
-- and a limit to the number of recursive evaluations.
--
-- (cf. 'toDynamicWithDefn', 'deval', 'devl')
devaluate :: Typeable a => (Expr -> Expr) -> Int -> Defn -> Expr -> Maybe a
devaluate ee n fxpr e  =  toDynamicWithDefn ee n fxpr e >>= fromDynamic

-- | Evaluates an 'Expr' expression into a regular Haskell value
--   using a 'Defn' definition when it is found in the given expression.
--   If there's a type-mismatch, this function return a default value.
--
-- This function requires a 'Expr'-deep-reencoding function
-- and a limit to the number of recursive evaluations.
--
-- (cf. 'toDynamicWithDefn', 'devaluate', devl')
deval :: Typeable a => (Expr -> Expr) -> Int -> Defn -> a -> Expr -> a
deval ee n fxpr x  =  fromMaybe x . devaluate ee n fxpr

-- | Like 'deval' but only works for when the given 'Defn' definition
--   has no case breakdowns.
--
-- In other words, this only works when the given 'Defn' is a singleton list
-- whose first value of the only element is a simple application without
-- constructors.
devalFast :: Typeable a => (Expr -> Expr) -> Int -> Defn -> a -> Expr -> a
devalFast _ n [defn] x  =  reval defn n x

-- | Evaluates an 'Expr' expression into a regular Haskell value
--   using a 'Defn' definition when it is found in the given expression.
--   If there's a type-mismatch, this raises an error.
--
-- This function requires a 'Expr'-deep-reencoding function
-- and a limit to the number of recursive evaluations.
--
-- (cf. 'toDynamicWithDefn', 'devaluate', deval')
devl :: Typeable a => (Expr -> Expr) -> Int -> Defn -> Expr -> a
devl ee n fxpr  =  deval ee n fxpr (error "devl: incorrect type?")

-- | Returns whether the given definition 'apparentlyTerminates'.
defnApparentlyTerminates :: Defn -> Bool
defnApparentlyTerminates [(efxs, e)]  =  apparentlyTerminates efxs e
defnApparentlyTerminates _  =  True

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
  anyAllEqual shovel  =  any (\bs -> allEqual bs && isDefined bs)
                      .  classifyOn fst
                      .  map (canonicalizeBndn . first shovel)
                      $  d

isRedundantByIntroduction :: Defn -> Bool
isRedundantByIntroduction d  =  any anyAllEqual [1..nArgs]
  where
  nArgs  =  length . tail . unfoldApp . fst $ head d
  anyAllEqual :: Int -> Bool
  anyAllEqual i  =  any (\bs -> allEqual bs && isCompleteDefn bs)
                 .  classifyOn fst
                 .  map (canonicalizeBndn . introduceVariableAt i)
                 $  d

-- | Introduces a hole at a given position in the binding:
--
-- > introduceVariableAt 1 (one -+- two, three -+- two)
-- (_ + 2 :: Int,3 + 2 :: Int)
--
-- > introduceVariableAt 2 (one -+- two, three -+- two)
-- (1 + _ :: Int,3 + _ :: Int)
--
-- Relevant occurrences are replaced.
--
-- TODO: find better examples for this comment
introduceVariableAt :: Int -> Bndn -> Bndn
introduceVariableAt i b@(l,r)  =
  if isVar p
  then b -- already a variable!
  else if any (`elem` vars (snd b')) (vars p)
  then (l, holeAsTypeOf r) -- cannot introduce, mark with hole
  else b'
  where
  p  =  l $!! i
  b'  =  unfoldPair (foldPair b // [(p,"introduced_var" `varAsTypeOf` p)])
-- TODO: replace "introduced_var" by something proper

isRedundantBySubsumption :: Defn -> Bool
isRedundantBySubsumption  =  is . map foldPair . filter isCompleteBndn
  -- above, we could have used noUnbound instead of isCompleteBndn
  -- we use isCompleteBndn as it is faster
  where
  is []  =  False
  is (b:bs)  =  any (b `isInstanceOf`) bs || is bs

-- | Returns whether the definition is complete,
--   i.e., whether it does not have any holes in the RHS.
isCompleteDefn :: Defn -> Bool
isCompleteDefn  =  all isCompleteBndn

-- | Returns whether the binding is complete,
--   i.e., whether it does not have any holes in the RHS.
isCompleteBndn :: Bndn -> Bool
isCompleteBndn (_,rhs)  =  isComplete rhs

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

canonicalizeBndn :: Bndn -> Bndn
canonicalizeBndn  =  unfoldPair . canonicalize . foldPair

-- | Returns whether a binding has undefined variables,
--   i.e.,
--   there are variables in the RHS that are not declared in the LHS.
--
-- > > hasUnbound (xx -:- xxs, xxs)
-- > False
--
-- > > hasUnbound (xx -:- xxs, yys)
-- > True
--
-- For 'Defn's, use 'isUndefined'.
hasUnbound :: Bndn -> Bool
hasUnbound  =  not . noUnbound

noUnbound :: Bndn -> Bool
noUnbound (lhs,rhs)  =  all (`elem` nubVars lhs) (vars rhs)

-- | Returns whether a 'Defn' has undefined variables,
--   i.e.,
--   there are variables in the RHSs that are not declared in the LHSs.
--
-- > > isUndefined [(nil, nil), (xx -:- xxs, xxs)]
-- > False
--
-- > > isUndefined [(nil, xxs), (xx -:- xxs, yys)]
-- > True
--
-- For single 'Bndn's, use 'hasUnbound'.
isUndefined :: Defn -> Bool
isUndefined  =  any hasUnbound

isDefined :: Defn -> Bool
isDefined  =  not . isUndefined
