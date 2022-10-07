-- |
-- Module      : Conjure.Engine
-- Copyright   : (c) 2021 Rudy Matela
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
  , property
  , properties
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

import Data.Dynamic (fromDyn, dynApp)

import Test.LeanCheck
import Test.LeanCheck.Tiers
import Test.LeanCheck.Error (errorToTrue, errorToFalse, errorToNothing)
import Test.LeanCheck.Utils (classifyOn)

import Test.Speculate.Reason (Thy, rules, equations, invalid, canReduceTo, printThy, closureLimit, doubleCheck)
import Test.Speculate.Engine (theoryFromAtoms, grounds, groundBinds, boolTy)

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
-- > squareSpec :: (Int -> Int) -> [Bool]
-- > squareSpec square  =  [ square 0 == 0
-- >                       , square 1 == 1
-- >                       , square 2 == 4
-- >                       ]
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
-- > import Test.LeanCheck (exists)
-- >
-- > squarePropertySpec :: (Int -> Int) -> [Bool]
-- > squarePropertySpec square  =  and
-- >   [ property $ \x -> square x >= x
-- >   , property $ \x -> square x >= 0
-- >   , property $ exists 60 $ \x -> square x > x
-- >   ]
conjureFromSpec :: Conjurable f => String -> (f -> [Bool]) -> [Prim] -> IO ()
conjureFromSpec  =  conjureFromSpecWith args


-- | Synthesizes an implementation from both a partial definition and a
--   function specification.
--
--   This works like the functions 'conjure' and 'conjureFromSpec' combined.
conjure0 :: Conjurable f => String -> f -> (f -> [Bool]) -> [Prim] -> IO ()
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
  , copyBindings          :: Bool -- ^ copy partial definition bindings in candidates
  , showTheory            :: Bool -- ^ show theory discovered by Speculate used in pruning
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
  , requireDescent         =  True
  , usePatterns            =  True
  , copyBindings           =  True
  , showTheory             =  False
  , uniqueCandidates       =  False
  }


-- | Like 'conjure' but allows setting options through 'Args'/'args'.
--
-- > conjureWith args{maxSize = 11} "function" function [...]
conjureWith :: Conjurable f => Args -> String -> f -> [Prim] -> IO ()
conjureWith args nm f  =  conjure0With args nm f (const [])

-- | Like 'conjureFromSpec' but allows setting options through 'Args'/'args'.
--
-- > conjureFromSpecWith args{maxSize = 11} "function" spec [...]
conjureFromSpecWith :: Conjurable f => Args -> String -> (f -> [Bool]) -> [Prim] -> IO ()
conjureFromSpecWith args nm p  =  conjure0With args nm undefined p

-- | Like 'conjure0' but allows setting options through 'Args'/'args'.
conjure0With :: Conjurable f => Args -> String -> f -> (f -> [Bool]) -> [Prim] -> IO ()
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
      (i:_)  ->  do let nc' = fromMaybe nc (findIndex (i==) cs)
                    putStrLn $ "-- tested " ++ show (t+nc'+1) ++ " candidates"
                    putStrLn $ showDefn i
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
conjpureFromSpec :: Conjurable f => String -> (f -> [Bool]) -> [Prim] -> ([[Defn]], [[Defn]], [Expr], Thy)
conjpureFromSpec  =  conjpureFromSpecWith args

-- | Like 'conjure0' but in the pure world.  (cf. 'conjpure')
conjpure0 :: Conjurable f => String -> f -> (f -> [Bool]) -> [Prim] -> ([[Defn]], [[Defn]], [Expr], Thy)
conjpure0 =  conjpure0With args

-- | Like 'conjpure' but allows setting options through 'Args' and 'args'.
conjpureWith :: Conjurable f => Args -> String -> f -> [Prim] -> ([[Defn]], [[Defn]], [Expr], Thy)
conjpureWith args nm f  =  conjpure0With args nm f (const [])

-- | Like 'conjureFromSpecWith' but in the pure world.  (cf. 'conjpure')
conjpureFromSpecWith :: Conjurable f => Args -> String -> (f -> [Bool]) -> [Prim] -> ([[Defn]], [[Defn]], [Expr], Thy)
conjpureFromSpecWith args nm p  =  conjpure0With args nm undefined p

-- | Like 'conjpure0' but allows setting options through 'Args' and 'args'.
--
-- This is where the actual implementation resides.  The functions
-- 'conjpure', 'conjpureWith', 'conjpureFromSpec', 'conjpureFromSpecWith',
-- 'conjure', 'conjureWith', 'conjureFromSpec', 'conjureFromSpecWith' and
-- 'conjure0' all refer to this.
conjpure0With :: Conjurable f => Args -> String -> f -> (f -> [Bool]) -> [Prim] -> ([[Defn]], [[Defn]], [Expr], Thy)
conjpure0With args@(Args{..}) nm f p es  =  (implementationsT, candidatesT, tests, thy)
  where
  tests  =  [ffxx //- bs | bs <- dbss]
  implementationsT  =  filterT implements candidatesT
  implements fx  =  defnApparentlyTerminates fx
                 && requal fx ffxx vffxx
                 && errorToFalse (and $ take maxTests $ p (cevl maxEvalRecursions fx))
  candidatesT  =  (if uniqueCandidates then nubCandidates args nm f else id)
               $  take maxSize candidatesTT
  (candidatesTT, thy)  =  candidateDefns args nm f p es
  ffxx   =  conjureApplication nm f
  vffxx  =  conjureVarApplication nm f
  (rrff:xxs)  =  unfoldApp vffxx

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
candidateDefns :: Conjurable f => Args -> String -> f -> (f -> [Bool]) -> [Prim] -> ([[Defn]], Thy)
candidateDefns args  =  candidateDefns' args
  where
  candidateDefns'  =  if usePatterns args
                      then candidateDefnsC
                      else candidateDefns1


nubCandidates :: Conjurable f => Args -> String -> f -> [[Defn]] -> [[Defn]]
nubCandidates Args{..} nm f  =  discardLaterT (===)
  where
  err  =  error "nubCandidates: unexpected evaluation error."
  eq  =  conjureDynamicEq f
  d1 === d2  =  all are $ take maxTests $ grounds (conjureTiersFor f) (conjureVarApplication nm f)
    where
    are :: Expr -> Bool
    are e  =  errorToFalse -- silences errors, ok since this is for optional measuring of optimal pruning
           $  (`fromDyn` err)
           $  eq `dynApp` fromMaybe err (toDynamicWithDefn (conjureExpress f) maxEvalRecursions d1 e)
                 `dynApp` fromMaybe err (toDynamicWithDefn (conjureExpress f) maxEvalRecursions d2 e)


-- | Return apparently unique candidate definitions
--   where there is a single body.
candidateDefns1 :: Conjurable f => Args -> String -> f -> (f -> [Bool]) -> [Prim] -> ([[Defn]], Thy)
candidateDefns1 args nm f _ ps  =  mapFst (mapT toDefn) $ candidateExprs args nm f ps
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
candidateDefnsC :: Conjurable f => Args -> String -> f -> (f -> [Bool]) -> [Prim] -> ([[Defn]], Thy)
candidateDefnsC Args{..} nm f p ps  =  (concatMapT fillingsFor fss,thy)
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

  -- Defn either passes the tests or fails with an error
  inoffensive :: Defn -> Bool
  inoffensive  =  and . map errorToTrue . take 12 . p . cevl maxEvalRecursions

  ps2fss :: [Expr] -> [[Defn]]
  ps2fss pats  =  filterT inoffensive
               .  discardT isRedundantDefn
               .  discardT (allEqual . map snd)
               .  products
               $  map p2eess pats
    where
    p2eess :: Expr -> [[Bndn]]
    -- the following guarded line is an optional optimization
    -- if the function is defined for the given pattern,
    -- simply use its return value as the only possible result
    p2eess pat | copyBindings && isGroundPat f pat  =  [[(pat, toValPat f pat)]]
    -- TODO: the "inoffensive" filter here should not be used for patterns that
    --       can be overridden by other patterns, e.g.:
    --
    --       > foo 0 = ...
    --       > foo x = ...
    --
    --       In the above case, the second pattern is never used for zero
    --       so whatever zero value is not offending to the property.
    p2eess pat  =  filterT (inoffensive . (:[]))
                .  mapT (pat,)
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

-- | Encodes a higher order property into a list of boolean results
property :: Testable a => a -> [Bool]
property  =  map snd . results

-- | Combines several 'property' results.
properties :: [[Bool]] -> [Bool]
properties  =  concat . transpose
