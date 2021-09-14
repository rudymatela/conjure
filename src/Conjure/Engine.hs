-- |
-- Module      : Conjure.Engine
-- Copyright   : (c) 2021 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- An internal module of 'Conjure',
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
  , conjure0
  , conjure0With
  , conjpure
  , conjpureWith
  , conjpure0
  , conjpure0With
  , candidateExprs
  , candidateDefns
  , candidateDefns1
  , candidateDefnsC
  , conjureTheory
  , conjureTheoryWith
  , deconstructions
  , module Data.Express
  , module Data.Express.Fixtures
  , module Test.Speculate.Engine
  , module Test.Speculate.Reason
  )
where

import Control.Monad (when)

import Data.Express
import Data.Express.Fixtures hiding ((-==-))
import qualified Data.Express.Triexpr as T

import Test.LeanCheck
import Test.LeanCheck.Tiers
import Test.LeanCheck.Error (errorToTrue, errorToFalse, errorToNothing)

import Test.Speculate.Reason (Thy, rules, equations, invalid, canReduceTo, printThy, closureLimit, doubleCheck)
import Test.Speculate.Engine (theoryFromAtoms, groundBinds, boolTy)

import Conjure.Expr
import Conjure.Conjurable
import Conjure.Prim
import Conjure.Defn


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
-- > -- testing 3 combinations of argument values
-- > -- looking through 3 candidates of size 1
-- > -- looking through 3 candidates of size 2
-- > -- looking through 5 candidates of size 3
-- > square x  =  x * x
--
-- The primitives list is defined with 'pr' and 'prim'.
conjure :: Conjurable f => String -> f -> [Prim] -> IO ()
conjure  =  conjureWith args

conjureProp :: Conjurable f => String -> (f -> Bool) -> [Prim] -> IO ()
conjureProp  =  conjurePropWith args

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
  , requireDescent        :: Bool -- ^ require recursive calls to deconstruct arguments
  , usePatterns           :: Bool -- ^ use pattern matching to create (recursive) candidates
  , showTheory            :: Bool -- ^ show theory discovered by Speculate used in pruning
  , forceTests            :: [[Expr]]  -- ^ force tests
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
args :: Args
args = Args
  { maxTests               =  360
  , maxSize                =  12
  , maxEvalRecursions      =  60
  , maxEquationSize        =   5
  , maxSearchTests         =  100000
  , maxDeconstructionSize  =   4
  , requireDescent         =  True
  , usePatterns            =  True
  , showTheory             =  False
  , forceTests             =  []
  }


-- | Like 'conjure' but allows setting options through 'Args'/'args'.
--
-- > conjureWith args{maxSize = 11} "function" function [...]
conjureWith :: Conjurable f => Args -> String -> f -> [Prim] -> IO ()
conjureWith args nm f  =  conjure0With args nm f (const True)

conjurePropWith :: Conjurable f => Args -> String -> (f -> Bool) -> [Prim] -> IO ()
conjurePropWith args nm p  =  conjure0With args nm undefined p

conjure0With :: Conjurable f => Args -> String -> f -> (f -> Bool) -> [Prim] -> IO ()
conjure0With args nm f p es  =  do
  print (var (head $ words nm) f)
  putStrLn $ "-- testing " ++ show (length ts) ++ " combinations of argument values"
  putStrLn $ "-- pruning with " ++ show nRules ++ "/" ++ show nREs ++ " rules"
  when (showTheory args) $ do
    putStrLn $ "{-"
    printThy thy
    putStrLn $ "-}"
  when (not . null $ invalid thy) $
    putStrLn $ "-- reasoning produced "
            ++ show (length (invalid thy)) ++ " incorrect properties,"
            ++ " please re-run with more tests for faster results"
  pr 1 rs
  where
  pr n []  =  putStrLn $ "cannot conjure\n"
  pr n ((is,cs):rs)  =  do
    putStrLn $ "-- looking through "
            ++ show (length cs)
            ++ " candidates of size " ++ show n
    -- when (n<=12) $ putStrLn $ unlines $ map showDefn cs
    case is of
      []     ->  pr (n+1) rs
      (i:_)  ->  putStrLn $ showDefn i
  rs  =  zip iss css
  (iss, css, ts, thy)  =  conjpureWith args nm f es
  nRules  =  length (rules thy)
  nREs  =  length (equations thy) + nRules


-- | Like 'conjure' but in the pure world.
--
-- Returns a triple with:
--
-- 1. tiers of implementations
-- 2. tiers of candidate bodies (right type)
-- 3. tiers of candidate expressions (any type)
-- 4. a list of tests
conjpure :: Conjurable f => String -> f -> [Prim] -> ([[Defn]], [[Defn]], [Expr], Thy)
conjpure =  conjpureWith args

conjpureProp :: Conjurable f => String -> (f -> Bool) -> [Prim] -> ([[Defn]], [[Defn]], [Expr], Thy)
conjpureProp =  conjpurePropWith args

conjpure0 :: Conjurable f => String -> f -> (f -> Bool) -> [Prim] -> ([[Defn]], [[Defn]], [Expr], Thy)
conjpure0 =  conjpure0With args

-- | Like 'conjpure' but allows setting options through 'Args' and 'args'.
conjpureWith :: Conjurable f => Args -> String -> f -> [Prim] -> ([[Defn]], [[Defn]], [Expr], Thy)
conjpureWith args nm f  =  conjpure0With args nm f (const True)

conjpurePropWith :: Conjurable f => Args -> String -> (f -> Bool) -> [Prim] -> ([[Defn]], [[Defn]], [Expr], Thy)
conjpurePropWith args nm p  =  conjpure0With args nm undefined p

-- | Like 'conjpure0' but allows setting options through 'Args' and 'args'.
conjpure0With :: Conjurable f => Args -> String -> f -> (f -> Bool) -> [Prim] -> ([[Defn]], [[Defn]], [Expr], Thy)
conjpure0With args@(Args{..}) nm f p es  =  (implementationsT, candidatesT, tests, thy)
  where
  tests  =  [ffxx //- bs | bs <- dbss]
  implementationsT  =  filterT implements candidatesT
  implements fx  =  defnApparentlyTerminates fx
                 && requal fx ffxx vffxx
                 && errorToFalse (p (cevl maxEvalRecursions fx))
  candidatesT  =  take maxSize candidatesTT
  (candidatesTT, thy)  =  candidateDefns args nm f es
  ffxx   =  conjureApplication nm f
  vffxx  =  conjureVarApplication nm f
  (rrff:xxs)  =  unfoldApp vffxx

  requal dfn e1 e2  =  isTrueWhenDefined dfn (e1 -==- e2)
  (-==-)  =  conjureMkEquation f

  isTrueWhenDefined dfn e  =  all (errorToFalse . deval (conjureExpress f) maxEvalRecursions dfn False)
                           $  map (e //-) dbss

  bss, dbss :: [[(Expr,Expr)]]
  bss  =  take maxSearchTests $ groundBinds (conjureTiersFor f) ffxx
  fbss  =  [zip xxs vs | vs <- forceTests, isWellTyped $ foldApp (rrff:vs)]
  dbss  =  take maxTests
        $  ([bs | bs <- bss, errorToFalse . eval False $ e //- bs] \\ fbss)
        ++ fbss
    where
    e  =  ffxx -==- ffxx


conjureTheory :: Conjurable f => String -> f -> [Prim] -> IO ()
conjureTheory  =  conjureTheoryWith args


conjureTheoryWith :: Conjurable f => Args -> String -> f -> [Prim] -> IO ()
conjureTheoryWith args nm f es  =  do
  putStrLn $ "theory with " ++ (show . length $ rules thy) ++ " rules and "
                            ++ (show . length $ equations thy) ++ " equations"
  printThy thy
  where
  (_, _, _, thy)  =  conjpureWith args nm f es


-- | Return apparently unique candidate definitions.
candidateDefns :: Conjurable f => Args -> String -> f -> [Prim] -> ([[Defn]], Thy)
candidateDefns args  =  cds args
  where
  cds  =  if usePatterns args
          then candidateDefnsC
          else candidateDefns1


-- | Return apparently unique candidate definitions
--   where there is a single body.
candidateDefns1 :: Conjurable f => Args -> String -> f -> [Prim] -> ([[Defn]], Thy)
candidateDefns1 args nm f ps  =  mapFst (mapT toDefn) $ candidateExprs args nm f ps
  where
  mapFst f (x,y)  =  (f x, y)
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
  keep  =  isRootNormalE thy . fastMostGeneralVariation
  ds  =  filter (conjureIsDeconstructor f maxTests) es
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
candidateDefnsC Args{..} nm f ps  =  (concatMapT fillingsFor fss,thy)
  where
  pats  =  conjurePats es nm f
  fss  =  concatMapT ps2fss pats
  es  =  map fst ps

  eh  =  holeAsTypeOf efxs
  efxs  =  conjureVarApplication nm f
  (ef:exs)  =  unfoldApp efxs

  keep  =  isRootNormal thy . fastMostGeneralVariation

  appsWith :: Expr -> [Expr] -> [[Expr]]
  appsWith eh vs  =  enumerateAppsFor eh keep $ vs ++ es


  ps2fss :: [Expr] -> [[Defn]]
  ps2fss pats  =  discardT (allEqual . map snd) . products $ map p2eess pats
    where
    p2eess :: Expr -> [[Bndn]]
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

  ds  =  filter (conjureIsDeconstructor f maxTests) es
  keepR ep | requireDescent  =  descends isDecOf ep
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
                     $  concatMapT (`appsWith` vars ep) [hs]
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


-- | Returns whether the given recursive call
--   deconstructs one of its arguments.
--
-- > > deconstructs1 ... (factorial' (dec' xx))
-- > True
--
-- > > deconstructs1 ... (factorial' (xx -+- one))
-- > False
--
-- > > deconstructs1 ... (xxs -++- yys)
-- > False
--
-- > > deconstructs1 ... (xxs -++- tail' yys)
-- > True
--
-- > > deconstructs1 ... (zero-:-xxs -++- tail' yys)
-- > True
--
-- 'deconstructs1' implies 'descends'.
deconstructs1 :: (Expr -> Bool) -> Expr -> Expr -> Bool
deconstructs1 isDec _ e  =  any isDeconstruction exs
  where
  (ef:exs)  =  unfoldApp e
  isDeconstruction e  =  not (null cs) && all isDec cs
    where
    cs  =  consts e

-- | Returns whether a non-empty subset of arguments
--   descends arguments by deconstruction.
--
-- > > descends isDec (xxs -++- yys) (xxs -++- tail' yys)
-- > True
--
-- > > descends isDec (xxs -++- yys) (xxs -++- yys)
-- > False
--
-- > > descends isDec (xxs -++- yys) (head' xxs -:- tail xxs  -++-  head' yys -:- tail yys)
-- > False

-- > > descends isDec (xxs -\/- yys) (yys -\/- tail' xxs)
-- > True
--
-- The following are not so obvious:
--
-- > > descends isDec (xxs -++- yys) (tail' yys -++- yys)
-- > False
--
-- > > descends isDec (xxs -++- yys) (xx-:-xxs -++- tail' yys)
-- > True
--
-- For all possible sets of arguments (2^n - 1 elements: 1 3 7 15 31),
-- see if any projects the same variables while only using deconstructions
-- and where there is at least a single deconstruction.
descends :: (Expr -> Expr -> Bool) -> Expr -> Expr -> Bool
descends isDecOf e' e  =  any d1 ss
  where
  desc  =  any d1 . uncurry useMatches . unzip
  d1 exys  =  all isNotConstruction exys
           && any isDeconstruction exys
  ss  =  init $ sets exys
  exys  =  zip exs eys
  (_:exs)  =  unfoldApp e'
  (_:eys)  =  unfoldApp e
  isDeconstruction (p,e) | isVar p    =  e `isDecOf` p
                         | otherwise  =  size e < size p
    where
    cs  =  consts e
  isNotConstruction (p,e) | isVar p    =  e == p || e `isDecOf` p
                          | otherwise  =  size e <= size p -- TODO: allow filter and id somehow
-- TODO: improve this function with better isNotConstruction


candidatesTD :: (Expr -> Bool) -> Expr -> [Expr] -> [[Expr]]
candidatesTD keep h primitives  =  filterT (not . hasHole)
                                $  town [[h]]
  where
  most = mostGeneralCanonicalVariation

  town :: [[Expr]] -> [[Expr]]
  town ((e:es):ess) | keep (most e)  =  [[e]] \/ town (expand e \/ (es:ess))
                    | otherwise      =  town (es:ess)
  town ([]:ess)  =  []:town ess
  town []  =  []

  expand :: Expr -> [[Expr]]
  expand e  =  case holesBFS e of
    [] -> []
    (h:_) -> mapT (fillBFS e) (replacementsFor h)

  replacementsFor :: Expr -> [[Expr]]
  replacementsFor h  =  filterT (\e -> typ e == typ h)
                     $  primitiveApplications primitives


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



--- normality checks ---

isRootNormal :: Thy -> Expr -> Bool
isRootNormal thy e  =  null $ T.lookup e trie
  where
  trie  =  T.fromList (rules thy)

isRootNormalE :: Thy -> Expr -> Bool
isRootNormalE thy e  =  isRootNormal thy e
                    &&  null (filter (e ->-) [e2 //- bs | (_,bs,e2) <- T.lookup e trie])
  where
  trie  =  T.fromList $ equations thy ++ map swap (equations thy)
  (->-)  =  canReduceTo thy


--- tiers utils ---

productsWith :: ([a] -> a) -> [ [[a]] ] -> [[a]]
productsWith f  =  mapT f . products
-- TODO: move to LeanCheck?

delayedProductsWith :: ([a] -> a) -> [ [[a]] ] -> [[a]]
delayedProductsWith f xsss  =  productsWith f xsss `addWeight` length xsss
-- TODO: move to LeanCheck?

foldAppProducts :: Expr -> [ [[Expr]] ] -> [[Expr]]
foldAppProducts ef  =  delayedProductsWith (foldApp . (ef:))
