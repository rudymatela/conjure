-- |
-- Module      : Conjure.Engine
-- Copyright   : (c) 2021-2024 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- An internal module of "Conjure",
-- a library for Conjuring function implementations
-- from tests or partial definitions.
-- (a.k.a.: functional inductive programming)
{-# LANGUAGE CPP, RecordWildCards, TupleSections #-}
module Conjure.Engine
  ( conjure
  , conjureWithMaxSize
  , Args(..)
  , args
  , conjureWith
  , conjureFromSpec
  , conjureFromSpecWith
  , conjure0
  , conjure0With
  , conjpure
  , conjpureWith
  , conjpureFromSpec
  , conjpureFromSpecWith
  , conjpure0
  , conjpure0With
  , candidateExprs
  , candidateDefns
  , candidateDefns1
  , candidateDefnsC
  , conjureTheory
  , conjureTheoryWith
  , module Data.Express
  , module Data.Express.Fixtures
  , module Conjure.Reason
  )
where

import Control.Monad (when)

import Data.Express
import Data.Express.Fixtures hiding ((-==-))

import Test.LeanCheck
import Test.LeanCheck.Tiers
import Test.LeanCheck.Error (errorToFalse)

import Conjure.Expr
import Conjure.Conjurable
import Conjure.Prim
import Conjure.Defn
import Conjure.Defn.Redundancy
import Conjure.Defn.Test
import Conjure.Red
import Conjure.Reason


-- | Conjures an implementation of a partially defined function.
--
-- Takes a 'String' with the name of a function,
-- a partially-defined function from a conjurable type,
-- and a list of building blocks encoded as 'Expr's.
--
-- For example, given:
--
-- > square :: Int -> Int
-- > square 0  =  0
-- > square 1  =  1
-- > square 2  =  4
-- >
-- > primitives :: [Prim]
-- > primitives =
-- >   [ pr (0::Int)
-- >   , pr (1::Int)
-- >   , prim "+" ((+) :: Int -> Int -> Int)
-- >   , prim "*" ((*) :: Int -> Int -> Int)
-- >   ]
--
-- The conjure function does the following:
--
-- > > conjure "square" square primitives
-- > square :: Int -> Int
-- > -- pruning with 14/25 rules
-- > -- testing 3 combinations of argument values
-- > -- looking through 3 candidates of size 1
-- > -- looking through 3 candidates of size 2
-- > -- looking through 5 candidates of size 3
-- > square x  =  x * x
--
-- The primitives list is defined with 'pr' and 'prim'.
conjure :: Conjurable f => String -> f -> [Prim] -> IO ()
conjure  =  conjureWith args


-- | Conjures an implementation from a function specification.
--
-- This function works like 'conjure' but instead of receiving a partial definition
-- it receives a boolean filter / property about the function.
--
-- For example, given:
--
-- > squareSpec :: (Int -> Int) -> Bool
-- > squareSpec square  =  square 0 == 0
-- >                    && square 1 == 1
-- >                    && square 2 == 4
--
-- Then:
--
-- > > conjureFromSpec "square" squareSpec primitives
-- > square :: Int -> Int
-- > -- pruning with 14/25 rules
-- > -- looking through 3 candidates of size 1
-- > -- looking through 4 candidates of size 2
-- > -- looking through 9 candidates of size 3
-- > square x  =  x * x
--
-- This allows users to specify QuickCheck-style properties,
-- here is an example using LeanCheck:
--
-- > import Test.LeanCheck (holds, exists)
-- >
-- > squarePropertySpec :: (Int -> Int) -> Bool
-- > squarePropertySpec square  =  and
-- >   [ holds n $ \x -> square x >= x
-- >   , holds n $ \x -> square x >= 0
-- >   , exists n $ \x -> square x > x
-- >   ]  where  n = 60
conjureFromSpec :: Conjurable f => String -> (f -> Bool) -> [Prim] -> IO ()
conjureFromSpec  =  conjureFromSpecWith args


-- | Synthesizes an implementation from both a partial definition and a
--   function specification.
--
--   This works like the functions 'conjure' and 'conjureFromSpec' combined.
conjure0 :: Conjurable f => String -> f -> (f -> Bool) -> [Prim] -> IO ()
conjure0  =  conjure0With args


-- | Like 'conjure' but allows setting the maximum size of considered expressions
--   instead of the default value of 12.
--
-- > conjureWithMaxSize 10 "function" function [...]
conjureWithMaxSize :: Conjurable f => Int -> String -> f -> [Prim] -> IO ()
conjureWithMaxSize sz  =  conjureWith args
                       {  maxSize = sz
                       ,  maxEquationSize = min sz (maxEquationSize args)
                       }


-- | Arguments to be passed to 'conjureWith' or 'conjpureWith'.
--   See 'args' for the defaults.
data Args = Args
  { maxTests              :: Int  -- ^ maximum number of tests to each candidate
  , maxSize               :: Int  -- ^ maximum size of candidate bodies
  , maxEvalRecursions     :: Int  -- ^ maximum number of recursive evaluations when testing candidates
  , maxEquationSize       :: Int  -- ^ maximum size of equation operands
  , maxSearchTests        :: Int  -- ^ maximum number of tests to search for defined values
  , maxDeconstructionSize :: Int  -- ^ maximum size of deconstructions (e.g.: @_ - 1@)

  -- advanced options --
  , carryOn               :: Bool -- ^ whether to carry on after finding a suitable candidate
  , showTheory            :: Bool -- ^ show theory discovered by Speculate used in pruning
  , usePatterns           :: Bool -- ^ use pattern matching to create (recursive) candidates

  -- pruning options --
  , rewriting             :: Bool -- ^ unique-modulo-rewriting candidates
  , requireDescent        :: Bool -- ^ require recursive calls to deconstruct arguments
  , adHocRedundancy       :: Bool -- ^ ad-hoc redundancy checks
  , copyBindings          :: Bool -- ^ copy partial definition bindings in candidates
  , atomicNumbers         :: Bool -- ^ restrict constant/ground numeric expressions to atoms
  , uniqueCandidates      :: Bool -- ^ unique-modulo-testing candidates
  }


-- | Default arguments to conjure.
--
-- * 60 tests
-- * functions of up to 12 symbols
-- * maximum of one recursive call allowed in candidate bodies
-- * maximum evaluation of up to 60 recursions
-- * pruning with equations up to size 5
-- * search for defined applications for up to 100000 combinations
-- * require recursive calls to deconstruct arguments
-- * don't show the theory used in pruning
-- * do not make candidates unique module testing
args :: Args
args = Args
  { maxTests               =  360
  , maxSize                =  12
  , maxEvalRecursions      =  60
  , maxEquationSize        =   5
  , maxSearchTests         =  100000
  , maxDeconstructionSize  =   4

  -- advanced options --
  , carryOn                =  False
  , showTheory             =  False
  , usePatterns            =  True

  -- pruning options --
  , rewriting              =  True
  , requireDescent         =  True
  , adHocRedundancy        =  True
  , copyBindings           =  True
  , atomicNumbers          =  True
  , uniqueCandidates       =  False
  }


-- | Like 'conjure' but allows setting options through 'Args'/'args'.
--
-- > conjureWith args{maxSize = 11} "function" function [...]
conjureWith :: Conjurable f => Args -> String -> f -> [Prim] -> IO ()
conjureWith args nm f  =  conjure0With args nm f (const True)

-- | Like 'conjureFromSpec' but allows setting options through 'Args'/'args'.
--
-- > conjureFromSpecWith args{maxSize = 11} "function" spec [...]
conjureFromSpecWith :: Conjurable f => Args -> String -> (f -> Bool) -> [Prim] -> IO ()
conjureFromSpecWith args nm p  =  conjure0With args nm undefined p

-- | Like 'conjure0' but allows setting options through 'Args'/'args'.
conjure0With :: Conjurable f => Args -> String -> f -> (f -> Bool) -> [Prim] -> IO ()
conjure0With args nm f p es  =  do
  print (var (head $ words nm) f)
  when (length ts > 0) $
    putStrLn $ "-- testing " ++ show (length ts) ++ " combinations of argument values"
  putStrLn $ "-- pruning with " ++ show nRules ++ "/" ++ show nREs ++ " rules"
  when (showTheory args) $ do
    putStrLn $ "{-"
    printThy thy
    putStrLn $ "-}"
  when (not . null $ invalid thy) $ do
    putStrLn $ "-- reasoning produced "
            ++ show (length (invalid thy)) ++ " incorrect properties,"
            ++ " please re-run with more tests for faster results"
    when (showTheory args) $ do
      putStrLn $ "{-"
      putStrLn $ "invalid:"
      putStr   $ unlines $ map showEq $ invalid thy
      putStrLn $ "-}"
  pr 1 0 rs
  where
  showEq eq  =  showExpr (fst eq) ++ " == " ++ showExpr (snd eq)
  pr n t []  =  do putStrLn $ "-- tested " ++ show t ++ " candidates"
                   putStrLn $ "cannot conjure\n"
  pr n t ((is,cs):rs)  =  do
    let nc  =  length cs
    putStrLn $ "-- looking through " ++ show nc ++ " candidates of size " ++ show n
    -- when (n<=12) $ putStrLn $ unlines $ map showDefn cs
    case is of
      []     ->  pr (n+1) (t+nc) rs
      (_:_)  ->  do pr1 t is cs
                    when (carryOn args) $ pr (n+1) (t+nc) rs
  pr1 t [] cs  =  return ()
  pr1 t (i:is) cs  =  do
    let (cs',cs'') = break (i==) cs
    let t' = t + length cs' + 1
    putStrLn $ "-- tested " ++ show t' ++ " candidates"
    putStrLn $ showDefn i
    when (carryOn args) $ pr1 t' is (drop 1 cs'')
  rs  =  zip iss css
  (iss, css, ts, thy)  =  conjpure0With args nm f p es
  nRules  =  length (rules thy)
  nREs  =  length (equations thy) + nRules


-- | Like 'conjure' but in the pure world.
--
-- Returns a quadruple with:
--
-- 1. tiers of implementations
-- 2. tiers of candidates
-- 3. a list of tests
-- 4. the underlying theory
conjpure :: Conjurable f => String -> f -> [Prim] -> ([[Defn]], [[Defn]], [Expr], Thy)
conjpure =  conjpureWith args

-- | Like 'conjureFromSpec' but in the pure world.  (cf. 'conjpure')
conjpureFromSpec :: Conjurable f => String -> (f -> Bool) -> [Prim] -> ([[Defn]], [[Defn]], [Expr], Thy)
conjpureFromSpec  =  conjpureFromSpecWith args

-- | Like 'conjure0' but in the pure world.  (cf. 'conjpure')
conjpure0 :: Conjurable f => String -> f -> (f -> Bool) -> [Prim] -> ([[Defn]], [[Defn]], [Expr], Thy)
conjpure0 =  conjpure0With args

-- | Like 'conjpure' but allows setting options through 'Args' and 'args'.
conjpureWith :: Conjurable f => Args -> String -> f -> [Prim] -> ([[Defn]], [[Defn]], [Expr], Thy)
conjpureWith args nm f  =  conjpure0With args nm f (const True)

-- | Like 'conjureFromSpecWith' but in the pure world.  (cf. 'conjpure')
conjpureFromSpecWith :: Conjurable f => Args -> String -> (f -> Bool) -> [Prim] -> ([[Defn]], [[Defn]], [Expr], Thy)
conjpureFromSpecWith args nm p  =  conjpure0With args nm undefined p

-- | Like 'conjpure0' but allows setting options through 'Args' and 'args'.
--
-- This is where the actual implementation resides.  The functions
-- 'conjpure', 'conjpureWith', 'conjpureFromSpec', 'conjpureFromSpecWith',
-- 'conjure', 'conjureWith', 'conjureFromSpec', 'conjureFromSpecWith' and
-- 'conjure0' all refer to this.
conjpure0With :: Conjurable f => Args -> String -> f -> (f -> Bool) -> [Prim] -> ([[Defn]], [[Defn]], [Expr], Thy)
conjpure0With args@(Args{..}) nm f p es  =  (implementationsT, candidatesT, tests, thy)
  where
  tests  =  [ffxx //- bs | bs <- dbss]
  implementationsT  =  filterT implements candidatesT
  implements fx  =  defnApparentlyTerminates fx
                 && requal fx ffxx vffxx
                 && errorToFalse (p (cevl maxEvalRecursions fx))
  candidatesT  =  (if uniqueCandidates then nubCandidates args nm f else id)
               $  take maxSize candidatesTT
  (candidatesTT, thy)  =  candidateDefns args nm f es
  ffxx   =  conjureApplication nm f
  vffxx  =  conjureVarApplication nm f

  requal dfn e1 e2  =  isTrueWhenDefined dfn (e1 -==- e2)
  (-==-)  =  conjureMkEquation f

  isTrueWhenDefined dfn e  =  all (errorToFalse . deval (conjureExpress f) maxEvalRecursions dfn False)
                           $  map (e //-) dbss

  bss, dbss :: [[(Expr,Expr)]]
  bss  =  take maxSearchTests $ groundBinds (conjureTiersFor f) ffxx
  dbss  =  take maxTests [bs | bs <- bss, errorToFalse . eval False $ e //- bs]
    where
    e  =  ffxx -==- ffxx


-- | Just prints the underlying theory found by "Test.Speculate"
--   without actually synthesizing a function.
conjureTheory :: Conjurable f => String -> f -> [Prim] -> IO ()
conjureTheory  =  conjureTheoryWith args


-- | Like 'conjureTheory' but allows setting options through 'Args'/'args'.
conjureTheoryWith :: Conjurable f => Args -> String -> f -> [Prim] -> IO ()
conjureTheoryWith args nm f es  =  do
  putStrLn $ "theory with " ++ (show . length $ rules thy) ++ " rules and "
                            ++ (show . length $ equations thy) ++ " equations"
  printThy thy
  where
  (_, _, _, thy)  =  conjpureWith args nm f es


-- | Return apparently unique candidate definitions.
candidateDefns :: Conjurable f => Args -> String -> f -> [Prim] -> ([[Defn]], Thy)
candidateDefns args  =  candidateDefns' args
  where
  candidateDefns'  =  if usePatterns args
                      then candidateDefnsC
                      else candidateDefns1


-- | Return apparently unique candidate definitions
--   where there is a single body.
candidateDefns1 :: Conjurable f => Args -> String -> f -> [Prim] -> ([[Defn]], Thy)
candidateDefns1 args nm f ps  =  first (mapT toDefn) $ candidateExprs args nm f ps
  where
  efxs  =  conjureVarApplication nm f
  toDefn e  =  [(efxs, e)]


-- | Return apparently unique candidate bodies.
candidateExprs :: Conjurable f => Args -> String -> f -> [Prim] -> ([[Expr]], Thy)
candidateExprs Args{..} nm f ps  =  (as \/ concatMapT (`enumerateFillings` recs) ts, thy)
  where
  es  =  map fst ps
  ts | typ efxs == boolTy  =  foldAppProducts andE [cs, rs]
                           \/ foldAppProducts orE  [cs, rs]
     | otherwise           =  filterT keepIf
                           $  foldAppProducts (conjureIf f) [cs, as, rs]
                           \/ foldAppProducts (conjureIf f) [cs, rs, as]
  cs  =  filterT (`notElem` [val False, val True])
      $  forN (hole (undefined :: Bool))
  as  =  forN efxs
  rs  =  forR efxs
  forN h  =  enumerateAppsFor h keep $ exs ++ es
  forR h  =  filterT (\e -> (eh `elem`) (holes e))
          $  enumerateAppsFor h keep $ exs ++ es ++ [eh]
  eh  =  holeAsTypeOf efxs
  efxs  =  conjureVarApplication nm f
  (ef:exs)  =  unfoldApp efxs
  keep | rewriting  =  isRootNormalC thy . fastMostGeneralVariation
       | otherwise  =  const True
  keepR | requireDescent  =  descends isDecOf efxs
        | otherwise       =  const True
    where
    e `isDecOf` e'  =  not $ null
                    [  ()
                    |  d <- deconstructions
                    ,  m <- maybeToList (e `match` d)
                    ,  filter (uncurry (/=)) m == [(holeAsTypeOf e', e')]
                    ]
    deconstructions :: [Expr]
    deconstructions  =  filter (conjureIsDeconstruction f maxTests)
                     $  concatMap candidateDeconstructionsFrom
                     $  concat . take maxDeconstructionSize
                     $  concatMapT forN [hs]
      where
      hs  =  nub $ conjureArgumentHoles f
  recs  =  filterT keepR
        $  foldAppProducts ef [forN h | h <- conjureArgumentHoles f]
  thy  =  doubleCheck (===)
       .  theoryFromAtoms (===) maxEquationSize . (:[]) . nub
       $  cjHoles (prim nm f:ps) ++ [val False, val True] ++ es
  (===)  =  cjAreEqual (prim nm f:ps) maxTests


-- | Return apparently unique candidate definitions
--   using pattern matching.
candidateDefnsC :: Conjurable f => Args -> String -> f -> [Prim] -> ([[Defn]], Thy)
candidateDefnsC Args{..} nm f ps  =  (discardT hasRedundant $ concatMapT fillingsFor fss,thy)
  where
  pats  =  conjurePats es nm f
  fss  =  concatMapT ps2fss pats
  es  =  map fst ps

  eh  =  holeAsTypeOf efxs
  efxs  =  conjureVarApplication nm f
  (ef:_)  =  unfoldApp efxs

  keep | rewriting  =  isRootNormalC thy . fastMostGeneralVariation
       | otherwise  =  const True

  appsWith :: Expr -> [Expr] -> [[Expr]]
  appsWith eh vs  =  enumerateAppsFor eh k $ vs ++ es
    where
    k | atomicNumbers && conjureIsNumeric f eh  =  \e -> keepNumeric e && keep e
      | otherwise                               =  keep
    -- discards non-atomic numeric ground expressions such as 1 + 1
    keepNumeric e  =  isFun e || isConst e || not (isGround e)

  isRedundant | adHocRedundancy  =  \e -> isRedundantDefn e || isRedundantModuloRewriting (normalize thy) e
              | otherwise        =  const False

  hasRedundant | adHocRedundancy  =  hasRedundantRecursion
               | otherwise        =  const False

  ps2fss :: [Expr] -> [[Defn]]
  ps2fss pats  =  discardT isRedundant
               .  products
               $  map p2eess pats
    where
    p2eess :: Expr -> [[Bndn]]
    -- the following guarded line is an optional optimization
    -- if the function is defined for the given pattern,
    -- simply use its return value as the only possible result
    p2eess pat | copyBindings && isGroundPat f pat  =  [[(pat, toValPat f pat)]]
    p2eess pat  =  mapT (pat,)
                .  appsWith pat
                .  tail
                $  vars pat ++ [eh | any (uncurry should) (zip aess aes)]
      where
      should aes ae  =  length (nub aes) > 1 && hasVar ae && (isApp ae || isUnbreakable ae)
      aes   =                  (tail . unfoldApp . rehole) pat
      aess  =  transpose $ map (tail . unfoldApp . rehole) pats

  fillingsFor1 :: Bndn -> [[Bndn]]
  fillingsFor1 (ep,er)  =  mapT (\es -> (ep,fill er es))
                        .  products
                        .  replicate (length $ holes er)
                        $  recs' ep

  fillingsFor :: Defn -> [[Defn]]
  fillingsFor  =  products . map fillingsFor1

  keepR ep | requireDescent  =  descends isDecOf ep
           | otherwise       =  const True
    where
    e `isDecOf` e'  =  not $ null
                    [  ()
                    |  d <- deconstructions
                    ,  m <- maybeToList (e `match` d)
                       -- h (_) is bound to e'
                    ,  lookup h m == Just e'
                       -- other than (h,e') we only accept (var,var)
                    ,  all (\(e1,e2) -> e1 == h || isVar e2) m
                    ]
      where
      h = holeAsTypeOf e'
    deconstructions :: [Expr]
    deconstructions  =  filter (conjureIsDeconstruction f maxTests)
                     $  concatMap candidateDeconstructionsFromHoled
                     $  concat . take maxDeconstructionSize
                     $  concatMapT (`appsWith` hs) [hs]
      where
      hs  =  nub $ conjureArgumentHoles f
  recs ep  =  filterT (keepR ep)
           .  discardT (\e -> e == ep)
           $  recsV' (tail (vars ep))
  recsV vs  =  filterT (\e -> any (`elem` vs) (vars e))
            $  foldAppProducts ef [appsWith h vs | h <- conjureArgumentHoles f]
  -- like recs, but memoized
  recs' ep  =  fromMaybe errRP $ lookup ep eprs
    where
    eprs = [(ep, recs ep) | ep <- possiblePats]
  possiblePats  =  nubSort . concat . concat $ pats
  -- like recsV, but memoized
  recsV' vs  =  fromMaybe errRV $ lookup vs evrs
    where
    evrs = [(vs, recsV vs) | vs <- nubSort $ map (tail . vars) possiblePats]

  thy  =  doubleCheck (===)
       .  theoryFromAtoms (===) maxEquationSize . (:[]) . nub
       $  cjHoles (prim nm f:ps) ++ [val False, val True] ++ es
  (===)  =  cjAreEqual (prim nm f:ps) maxTests
  isUnbreakable  =  conjureIsUnbreakable f
  errRP  =  error "candidateDefnsC: unexpected pattern.  You have found a bug, please report it."
  errRV  =  error "candidateDefnsC: unexpected variables.  You have found a bug, please report it."


-- | Checks if the given pattern is a ground pattern.
--
-- A pattern is a ground pattern when its arguments are fully defined
-- and evaluating the function returns a defined value.
--
-- This is to be used on values returned by conjurePats.
--
-- For now, this is only used on 'candidateDefnsC'.
isGroundPat :: Conjurable f => f -> Expr -> Bool
isGroundPat f pat  =  errorToFalse . eval False $ gpat -==- gpat
  where
  gpat  =  toGroundPat f pat
  (-==-)  =  conjureMkEquation f


-- | Given a complete "pattern", i.e. application encoded as expr,
--   converts it from using a "variable" function,
--   to an actual "value" function.
--
-- This function is used on 'isGroundPat' and 'toValPat'
toGroundPat :: Conjurable f => f -> Expr -> Expr
toGroundPat f pat  =  foldApp (value "f" f : tail (unfoldApp pat))

-- | Evaluates a pattern to its final value.
--
-- Only to be used when the function is defined for the given set of arguments.
--
-- For now, this is only used on 'candidateDefnsC'.
toValPat :: Conjurable f => f -> Expr -> Expr
toValPat f  =  conjureExpress f . toGroundPat f
-- NOTE: the use of conjureExpress above is a hack.
--       Here, one could have used a conjureVal function,
--       that lifts 'val' over 'Expr's.
--       However this function does not exist.


-- hardcoded filtering rules

keepIf :: Expr -> Bool
keepIf (Value "if" _ :$ ep :$ ex :$ ey)
  | ex == ey  =  False
  | anormal ep  =  False
  | otherwise  =  case binding ep of
                  Just (v,e) -> v `notElem` values ex
                  Nothing -> True
  where
  anormal (Value "==" _ :$ e1 :$ e2) | isVar e2 || isConst e1  =  True
  anormal _                                                    =  False
  binding :: Expr -> Maybe (Expr,Expr)
  binding (Value "==" _ :$ e1 :$ e2) | isVar e1   =  Just (e1,e2)
                                     | isVar e2   =  Just (e2,e1)
  binding _                                       =  Nothing
keepIf _  =  error "Conjure.Engine.keepIf: not an if"


-- equality between candidates

nubCandidates :: Conjurable f => Args -> String -> f -> [[Defn]] -> [[Defn]]
nubCandidates Args{..} nm f  =
  discardLaterT $ equalModuloTesting maxTests maxEvalRecursions nm f


--- tiers utils ---

productsWith :: ([a] -> a) -> [ [[a]] ] -> [[a]]
productsWith f  =  mapT f . products
-- TODO: move productsWith to LeanCheck?

delayedProductsWith :: ([a] -> a) -> [ [[a]] ] -> [[a]]
delayedProductsWith f xsss  =  productsWith f xsss `addWeight` length xsss
-- TODO: move delayedProductsWith to LeanCheck?

foldAppProducts :: Expr -> [ [[Expr]] ] -> [[Expr]]
foldAppProducts ef  =  delayedProductsWith (foldApp . (ef:))

boolTy :: TypeRep
boolTy  =  typ b_
