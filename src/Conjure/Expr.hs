-- |
-- Module      : Conjure.Expr
-- Copyright   : (c) 2021 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- This internal module reexports 'Data.Express' along with a few other
-- utilities.
{-# LANGUAGE CPP, TupleSections #-}
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
  , valuesBFS
  , holesBFS
  , fillBFS
  , showEq
  , lhs
  , rhs
  , ($$**)
  , ($$|<)
  , possibleHoles
  , isDeconstructionE
  , revaluate
  , reval

  , enumerateApps
  , enumerateAppsFor

  , module Conjure.Utils
  )
where

import Conjure.Utils

import Data.Express
import Data.Express.Utils.Typeable
import Data.Express.Fixtures hiding ((-==-))
import Data.Dynamic

import Test.LeanCheck (filterT, (\/), delay, productWith, productMaybeWith)

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
-- TODO: move >$$< into Data.Express?

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

-- lists terminal values in BFS order
valuesBFS :: Expr -> [Expr]
valuesBFS  =  concat . bfs
  where
  bfs :: Expr -> [[Expr]]
  bfs (ef :$ ex)  =  [] : mzip (bfs ef) (bfs ex)
  bfs e  =  [[e]]

-- lists holes in BFS order
holesBFS :: Expr -> [Expr]
holesBFS  =  filter isHole . valuesBFS

fillBFS :: Expr -> Expr -> Expr
fillBFS e e'  =  fst (f e)
  where
  f :: Expr -> (Expr,Maybe Int)
  f (ef :$ ex)  =  case (mf, mx) of
    (Nothing, Nothing)             -> (ef :$ ex, Nothing)
    (Just lf, Nothing)             -> (ef' :$ ex, Just $ lf+1)
    (Nothing, Just lx)             -> (ef :$ ex', Just $ lx+1)
    (Just lf, Just lx) | lf <= lx  -> (ef' :$ ex, Just $ lf+1)
                       | otherwise -> (ef :$ ex', Just $ lx+1)
    where
    (ef', mf)  =  f ef
    (ex', mx)  =  f ex
  f e | isHole e && typ e == typ e'  =  (e', Just 0)
      | otherwise                    =  (e, Nothing)
-- TODO: move BFS functions into Express?

showEq :: Expr -> String
showEq (((Value "==" _) :$ lhs) :$ rhs)  =  showExpr lhs ++ "  =  " ++ showExpr rhs
showEq e  =  "not an Eq: " ++ show e

lhs, rhs :: Expr -> Expr
lhs (((Value "==" _) :$ e) :$ _)  =  e
rhs (((Value "==" _) :$ _) :$ e)  =  e

-- Debug: application that always works
($$**) :: Expr -> Expr -> Maybe Expr
e1 $$** e2  =  Just $ e1 :$ e2

-- Debug: application that works for the correct kinds
($$|<) :: Expr -> Expr -> Maybe Expr
e1 $$|< e2  =  if isFunTy t1 && tyArity (argumentTy t1) == tyArity t2
               then Just $ e1 :$ e2
               else Nothing
  where
  t1  =  ktyp e1
  t2  =  ktyp e2

  ktyp :: Expr -> TypeRep
  ktyp (e1 :$ e2)  =  resultTy (ktyp e1)
  ktyp e  =  typ e

possibleHoles :: [Expr] -> [Expr]
possibleHoles  =  nubSort . ph . nubSort . map holeAsTypeOf
  where
  ph hs  =  case nubSort $ hs ++ [holeAsTypeOf hfx | hf <- hs, hx <- hs, Just hfx <- [hf $$ hx]] of
            hs' | hs' == hs -> hs
                | otherwise -> ph hs'
  nubSort  =  nub . sort -- TODO: this is O(n^2), make this O(n log n)


-- -- Expression enumeration -- --

enumerateAppsFor :: Expr -> (Expr -> Bool) -> [[Expr]] -> [[Expr]]
enumerateAppsFor  =  enumerateApps3For

enumerateApps :: (Expr -> Bool) -> [Expr] -> [[Expr]]
enumerateApps  =  enumerateApps1

enumerateApps1For :: Expr -> (Expr -> Bool) -> [Expr] -> [[Expr]]
enumerateApps1For h keep  =  filterT (\e -> typ e == typ h) . enumerateApps1 keep

enumerateApps1 :: (Expr -> Bool) -> [Expr] -> [[Expr]]
enumerateApps1 keep  =  exprT . (:[])
  where
  exprT ess  =  filterT keep
             $  ess \/ (delay $ productMaybeWith ($$) rss rss)
    where
    rss = exprT ess

enumerateApps2For :: Expr -> (Expr -> Bool) -> [Expr] -> [[Expr]]
enumerateApps2For h keep  =  map concat
                          .  filterT (\(e:_) -> typ e == typ h)
                          .  exprT
                          .  map (groupOn typ . sortOn typ)
                          .  (:[])
  where
  exprT :: [[[Expr]]] -> [[[Expr]]]
  exprT ess  =  ess \/ (delay $ productMaybeWith ($$**) rss rss)
    where
    rss = exprT ess
    efs $$** exs
      | isNothing (head efs $$ head exs)  =  Nothing
      | otherwise  =  case [ef :$ ex | ef <- efs, ex <- exs, keep (ef :$ ex)] of
                      [] -> Nothing
                      es -> Just es

enumerateApps3For :: Expr -> (Expr -> Bool) -> [[Expr]] -> [[Expr]]
enumerateApps3For h keep ess  =  for h
  where
  hs :: [Expr]
  hs  =  possibleHoles . concat $ take 1 ess
  for :: Expr -> [[Expr]]
  for h  =  filterT (\e -> typ h == typ e) ess \/ delay apps
    where
    apps  =  foldr (\/) []
          [  filterT keep $ productWith (:$) (for hf) (for hx)
          |  hf <- hs
          ,  hx <- hs
          ,  Just hfx <- [hf $$ hx]
          ,  typ h == typ hfx
          ]

-- Like 'isDeconstruction' but lifted over the 'Expr' type.
isDeconstructionE :: [Expr] -> Expr -> Expr -> Bool
--                   [a] -> (a -> Bool) -> (a -> a) -> Bool
isDeconstructionE [] _ _  =  error "isDeconstructionE: empty list of test values"
isDeconstructionE es ez ed | all isIllTyped [f :$ e | e <- es, f <- [ez,ed]]  =  error "isDeconstructionE: types do not match"
isDeconstructionE es ez ed  =  isDeconstruction es (eval False . (ez :$)) (ed :$)

recursiveToDynamic :: (Expr,Expr) -> Int -> Int -> Expr -> Maybe Dynamic
recursiveToDynamic (efxs, ebody) m n  =  fmap snd . re n
  where
  (ef':exs')  =  unfoldApp efxs
  re :: Int -> Expr -> Maybe (Int, Dynamic)
  re 0 _  =  error "recursiveToDynamic: recursion limit reached"
  re n (Value "if" _ :$ ec :$ ex :$ ey)  =  case evaluate ec of
    Nothing    -> Nothing
    Just True  -> re n ex
    Just False -> re n ey
  re n e  =  case unfoldApp e of
    [] -> error "recursiveToDynamic: empty application unfold"  -- should never happen
    [e] -> (1,) <$> toDynamic e
    (ef:exs) | ef == ef' -> re (n-1) $ ebody //- zip exs' exs
             | otherwise -> foldl1 ($$) (map (re n) (ef:exs))
  Just (n1,d1) $$ Just (n2,d2)  =  if n1+n2 <= m
                                   then (n1+n2,) <$> dynApply d1 d2
                                   else Nothing
  _ $$ _                        =  Nothing

revaluate :: Typeable a => (Expr,Expr) -> Int -> Int -> Expr -> Maybe a
revaluate dfn m n e  =  recursiveToDynamic dfn m n e >>= fromDynamic

reval :: Typeable a => (Expr,Expr) -> Int -> Int -> a -> Expr -> a
reval dfn m n x e = fromMaybe x (revaluate dfn m n e)
