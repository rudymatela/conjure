-- |
-- Module      : Conjure.Engine
-- Copyright   : (c) 2021-2025 Rudy Matela
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
  , Results(..)
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
import Conjure.Ingredient
import Conjure.Defn
import Conjure.Defn.Redundancy
import Conjure.Defn.Test
import Conjure.Red
import Conjure.Reason

import System.CPUTime (getCPUTime)


-- | Conjures an implementation of a partially defined function.
--
-- Takes a 'String' with the name of a function,
-- a partially-defined function from a conjurable type,
-- and a list of building blocks encoded as 'Expr's.
--
-- For example, given:
--
-- > factorial :: Int -> Int
-- > factorial 2  =  2
-- > factorial 3  =  6
-- > factorial 4  =  24
-- >
-- > ingredients :: [Ingredient]
-- > ingredients  =
-- >   [ con (0::Int)
-- >   , con (1::Int)
-- >   , fun "+" ((+) :: Int -> Int -> Int)
-- >   , fun "*" ((*) :: Int -> Int -> Int)
-- >   , fun "-" ((-) :: Int -> Int -> Int)
-- >   ]
--
-- The 'conjure' function does the following:
--
-- > > conjure "factorial" factorial ingredients
-- > factorial :: Int -> Int
-- > -- 0.1s, testing 4 combinations of argument values
-- > -- 0.8s, pruning with 27/65 rules
-- > -- ...  ...  ...  ...  ...  ...
-- > -- 0.9s, 35 candidates of size 6
-- > -- 0.9s, 167 candidates of size 7
-- > -- 0.9s, tested 95 candidates
-- > factorial 0  =  1
-- > factorial x  =  x * factorial (x - 1)
--
-- The ingredients list is defined with 'con' and 'fun'.
conjure :: Conjurable f => String -> f -> [Ingredient] -> IO ()
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
-- > > conjureFromSpec "square" squareSpec [fun "*" ((*) :: Int -> Int -> Int)]
-- > square :: Int -> Int
-- > -- 0.1s, pruning with 2/6 rules
-- > -- 0.1s, 1 candidates of size 1
-- > -- 0.1s, 0 candidates of size 2
-- > -- 0.1s, 1 candidates of size 3
-- > -- 0.1s, tested 2 candidates
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
conjureFromSpec :: Conjurable f => String -> (f -> Bool) -> [Ingredient] -> IO ()
conjureFromSpec  =  conjureFromSpecWith args


-- | Synthesizes an implementation from both a partial definition and a
--   function specification.
--
--   This works like the functions 'conjure' and 'conjureFromSpec' combined.
conjure0 :: Conjurable f => String -> f -> (f -> Bool) -> [Ingredient] -> IO ()
conjure0  =  conjure0With args


-- | Like 'conjure' but allows setting the maximum size of considered expressions
--   instead of the default value of 24.
--
-- > conjureWithMaxSize 12 "function" function [...]
--
-- This function is a candidate for going away.
-- Please set maxSize in Args instead.
conjureWithMaxSize :: Conjurable f => Int -> String -> f -> [Ingredient] -> IO ()
conjureWithMaxSize sz  =  conjureWith args
                       {  maxSize = sz
                       ,  maxEquationSize = min sz (maxEquationSize args)
                       }


-- | Arguments to be passed to 'conjureWith' or 'conjpureWith'.
--   See 'args' for the defaults.
data Args = Args
  { maxTests              :: Int  -- ^ maximum number of tests to each candidate
  , maxSize               :: Int  -- ^ maximum size of candidate bodies
  , target                :: Int  -- ^ enumerate further sizes of candidates until this target
  , maxEvalRecursions     :: Int  -- ^ maximum number of recursive evaluations when testing candidates
  , maxEquationSize       :: Int  -- ^ maximum size of equation operands
  , maxSearchTests        :: Int  -- ^ maximum number of tests to search for defined values
  , maxDeconstructionSize :: Int  -- ^ maximum size of deconstructions (e.g.: @_ - 1@)
  , maxConstantSize       :: Int  -- ^ maximum size of constants (0 for no limit)
  , maxPatternSize        :: Int  -- ^ maximum size of patterns (0 for no limit)
  , maxPatternDepth       :: Int  -- ^ maximum depth of patterns

  -- advanced & debug options --
  , carryOn               :: Bool -- ^ whether to carry on after finding a suitable candidate
  , showTheory            :: Bool -- ^ show theory discovered by Speculate used in pruning
  , usePatterns           :: Bool -- ^ use pattern matching to create (recursive) candidates
  , showRuntime           :: Bool -- ^ show runtime
  , showCandidates        :: Bool -- ^ (debug) show candidates -- warning: wall of text
  , showTests             :: Bool -- ^ (debug) show tests
  , showPatterns          :: Bool -- ^ (debug) show possible LHS patterns
  , showDeconstructions   :: Bool -- ^ (debug) show conjectured-and-allowed deconstructions

  -- pruning options --
  , rewriting             :: Bool -- ^ unique-modulo-rewriting candidates
  , requireDescent        :: Bool -- ^ require recursive calls to deconstruct arguments
  , adHocRedundancy       :: Bool -- ^ ad-hoc redundancy checks
  , copyBindings          :: Bool -- ^ copy partial definition bindings in candidates
  , earlyTests            :: Bool -- ^ perform tests early-and-independently on each binding
  , atomicNumbers         :: Bool -- ^ restrict constant/ground numeric expressions to atoms
  , requireZero           :: Bool -- ^ require 0 as base case for Num recursions
  , uniqueCandidates      :: Bool -- ^ unique-modulo-testing candidates
  }


-- | Default arguments to conjure.
--
-- * 60 tests
-- * functions of up to 24 symbols
-- * target testing over 50400 candidates
-- * maximum of one recursive call allowed in candidate bodies
-- * maximum evaluation of up to 60 recursions
-- * pruning with equations up to size 5
-- * search for defined applications for up to 100000 combinations
-- * require recursive calls to deconstruct arguments
-- * don't show the theory used in pruning
-- * do not show tested candidates
-- * do not make candidates unique module testing
args :: Args
args = Args
  { maxTests               =  360
  , maxSize                =  24
  , target                 =  10080
  , maxEvalRecursions      =  60
  , maxEquationSize        =   5
  , maxSearchTests         =  110880
  , maxDeconstructionSize  =   4
  , maxConstantSize        =   0 -- unlimited
  , maxPatternSize         =   0 -- unlimited
  , maxPatternDepth        =   1

  -- advanced & debug options --
  , carryOn                =  False
  , showTheory             =  False
  , usePatterns            =  True
  , showRuntime            =  True
  , showCandidates         =  False
  , showTests              =  False
  , showDeconstructions    =  False
  , showPatterns           =  False

  -- pruning options --
  , rewriting              =  True
  , requireDescent         =  True
  , adHocRedundancy        =  True
  , copyBindings           =  True
  , earlyTests             =  True
  , atomicNumbers          =  True
  , requireZero            =  False
  , uniqueCandidates       =  False
  }

-- TODO: remove the requireZero option from args?


-- | Like 'conjure' but allows setting options through 'Args'/'args'.
--
-- > conjureWith args{maxSize = 18} "function" function [...]
conjureWith :: Conjurable f => Args -> String -> f -> [Ingredient] -> IO ()
conjureWith args nm f  =  conjure0With args nm f (const True)

-- | Like 'conjureFromSpec' but allows setting options through 'Args'/'args'.
--
-- > conjureFromSpecWith args{maxSize = 18} "function" spec [...]
conjureFromSpecWith :: Conjurable f => Args -> String -> (f -> Bool) -> [Ingredient] -> IO ()
conjureFromSpecWith args nm p  =  conjure0With args nm undefined p

-- | Like 'conjure0' but allows setting options through 'Args'/'args'.
conjure0With :: Conjurable f => Args -> String -> f -> (f -> Bool) -> [Ingredient] -> IO ()
conjure0With args nm f p es  =  do
  -- the code section below became quite ugly with time and patches.
  -- it is still maintainable and readable as it is, but perhaps
  -- needs to be cleaned up and simplified
  t0 <- if showRuntime args
        then getCPUTime
        else return (-1)
  print (var (head $ words nm) f)
  when (length ts > 0) $ do
    putWithTimeSince t0 $ "testing " ++ show (length ts) ++ " combinations of argument values"
    when (showTests args) $ do
      putStrLn $ "{-"
      putStr $ showDefn ts
      putStrLn $ "-}"
  if length ts == 0 && errorToFalse (p undefined)
  then putStrLn $ nm ++ "  =  error \"could not reify specification, suggestion: conjureFromSpec\"\n"
  else do
    putWithTimeSince t0 $ "pruning with " ++ show nRules ++ "/" ++ show nREs ++ " rules"
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
    when (showPatterns args) $ do
      putStr $ unlines
             $ zipWith (\i -> (("-- allowed patterns of size " ++ show i ++ "\n{-\n") ++) . (++ "-}") . unlines) [1..]
             $ mapT showDefn
             $ patternss results
    when (showDeconstructions args) $ do
      putStrLn $ "{- List of allowed deconstructions:"
      putStr   $ unlines $ map show $ deconstructions results
      putStrLn $ "-}"
    pr t0 1 0 rs
  where
  showEq eq  =  showExpr (fst eq) ++ " == " ++ showExpr (snd eq)
  pr :: Integer -> Int -> Int -> [([Defn], [Defn])] -> IO ()
  pr t0 n t []  =  do putWithTimeSince t0 $ "tested " ++ show t ++ " candidates"
                      putStrLn $ nm ++ "  =  undefined  -- search exhausted\n"
  pr t0 n t ((is,cs):rs)  =  do
    let nc  =  length cs
    putWithTimeSince t0 $ show nc ++ " candidates of size " ++ show n
    when (showCandidates args) $
      putStr $ unlines $ ["{-"] ++ map showDefn cs ++ ["-}"]
    case is of
      []     ->  pr t0 (n+1) (t+nc) rs
      (_:_)  ->  do pr1 t is cs
                    when (carryOn args) $ pr t0 (n+1) (t+nc) rs
    where
    pr1 t [] cs  =  return ()
    pr1 t (i:is) cs  =  do
      let (cs',cs'') = break (i==) cs
      let t' = t + length cs' + 1
      putWithTimeSince t0 $ "tested " ++ show t' ++ " candidates"
      putStrLn $ showDefn i
      when (carryOn args) $ pr1 t' is (drop 1 cs'')
  rs  =  zip iss css
  results  =  conjpure0With args nm f p es
  iss  =  implementationss results
  css  =  candidatess results
  ts   =  bindings results
  thy  =  theory results
  nRules  =  length (rules thy)
  nREs  =  length (equations thy) + nRules


-- | Results to the 'conjpure' family of functions.
-- This is for advanced users.
-- One is probably better-off using the 'conjure' family.
data Results = Results
  { implementationss :: [[Defn]] -- ^ tiers of implementations
  , candidatess :: [[Defn]]      -- ^ tiers of candidates
  , bindings :: Defn             -- ^ test bindings used to verify candidates
  , theory :: Thy                -- ^ the underlying theory
  , patternss :: [[Defn]]        -- ^ tiers of allowed patterns
  , deconstructions :: [Expr]    -- ^ the list of allowed deconstructions
  }


-- | Like 'conjure' but in the pure world.
--
-- The most important part of the result are the tiers of implementations
-- however results also include candidates, tests and the underlying theory.
conjpure :: Conjurable f => String -> f -> [Ingredient] -> Results
conjpure =  conjpureWith args

-- | Like 'conjureFromSpec' but in the pure world.  (cf. 'conjpure')
conjpureFromSpec :: Conjurable f => String -> (f -> Bool) -> [Ingredient] -> Results
conjpureFromSpec  =  conjpureFromSpecWith args

-- | Like 'conjure0' but in the pure world.  (cf. 'conjpure')
conjpure0 :: Conjurable f => String -> f -> (f -> Bool) -> [Ingredient] -> Results
conjpure0 =  conjpure0With args

-- | Like 'conjpure' but allows setting options through 'Args' and 'args'.
conjpureWith :: Conjurable f => Args -> String -> f -> [Ingredient] -> Results
conjpureWith args nm f  =  conjpure0With args nm f (const True)

-- | Like 'conjureFromSpecWith' but in the pure world.  (cf. 'conjpure')
conjpureFromSpecWith :: Conjurable f => Args -> String -> (f -> Bool) -> [Ingredient] -> Results
conjpureFromSpecWith args nm p  =  conjpure0With args nm undefined p

-- | Like 'conjpure0' but allows setting options through 'Args' and 'args'.
--
-- This is where the actual implementation resides.  The functions
-- 'conjpure', 'conjpureWith', 'conjpureFromSpec', 'conjpureFromSpecWith',
-- 'conjure', 'conjureWith', 'conjureFromSpec', 'conjureFromSpecWith' and
-- 'conjure0' all refer to this.
conjpure0With :: Conjurable f => Args -> String -> f -> (f -> Bool) -> [Ingredient] -> Results
conjpure0With args@(Args{..}) nm f p es  =  Results
  { implementationss  =  implementationsT
  , candidatess  =  candidatesT
  , bindings  =  tests
  , theory  =  thy
  , patternss  =  patternss
  , deconstructions  =  deconstructions
  }
  where
  implementationsT  =  filterT implements candidatesT
  implements fx  =  defnApparentlyTerminates fx
                 && test fx
                 && errorToFalse (p (cevl maxEvalRecursions fx))
  candidatesT  =  (if uniqueCandidates then nubCandidates args nm f else id)
               $  (if target > 0 then targetiers target else id)
               $  (if maxSize > 0 then take maxSize else id)
               $  candidatesTT
  (candidatesTT, thy, patternss, deconstructions)  =  candidateDefns args nm f es

  test dfn  =  all (errorToFalse . deval (conjureExpress f) maxEvalRecursions dfn False)
            $  [funToVar lhs -==- rhs | (lhs, rhs) <- tests]
  tests  =  conjureTestDefn maxTests maxSearchTests nm f
  (-==-)  =  conjureMkEquation f


-- | Just prints the underlying theory found by "Test.Speculate"
--   without actually synthesizing a function.
conjureTheory :: Conjurable f => String -> f -> [Ingredient] -> IO ()
conjureTheory  =  conjureTheoryWith args


-- | Like 'conjureTheory' but allows setting options through 'Args'/'args'.
conjureTheoryWith :: Conjurable f => Args -> String -> f -> [Ingredient] -> IO ()
conjureTheoryWith args nm f es  =  do
  putStrLn $ "theory with " ++ (show . length $ rules thy) ++ " rules and "
                            ++ (show . length $ equations thy) ++ " equations"
  printThy thy
  where
  Results {theory = thy}  =  conjpureWith args nm f es


-- | Return apparently unique candidate definitions.
--
-- This function returns a trio:
--
-- 1. tiers of candidate definitions
-- 2. an equational theory
-- 3. a list of allowed deconstructions
candidateDefns :: Conjurable f => Args -> String -> f -> [Ingredient] -> ([[Defn]], Thy, [[Defn]], [Expr])
candidateDefns args  =  candidateDefns' args
  where
  candidateDefns'  =  if usePatterns args
                      then candidateDefnsC
                      else candidateDefns1


-- | Return apparently unique candidate definitions
--   where there is a single body.
candidateDefns1 :: Conjurable f => Args -> String -> f -> [Ingredient] -> ([[Defn]], Thy, [[Defn]], [Expr])
candidateDefns1 args nm f ps  =  first4 (mapT toDefn) $ candidateExprs args nm f ps
  where
  efxs  =  conjureVarApplication nm f
  toDefn e  =  [(efxs, e)]
  first4 f (x,y,z,w)  =  (f x, y, z, w)


-- | Return apparently unique candidate bodies.
candidateExprs :: Conjurable f => Args -> String -> f -> [Ingredient] -> ([[Expr]], Thy, [[Defn]], [Expr])
candidateExprs Args{..} nm f ps  =
  ( as \/ concatMapT (`enumerateFillings` recs) ts
  , thy
  , [[ [(efxs, eh)] ]]
  , deconstructions
  )
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
       $  cjHoles (fun nm f:ps) ++ [val False, val True] ++ es
  (===)  =  cjAreEqual (fun nm f:ps) maxTests


-- | Return apparently unique candidate definitions
--   using pattern matching.
candidateDefnsC :: Conjurable f => Args -> String -> f -> [Ingredient] -> ([[Defn]], Thy, [[Defn]], [Expr])
candidateDefnsC Args{..} nm f ps  =
  ( discardT hasRedundant $ concatMapT fillingsFor fss
  , thy
  , mapT (map (,eh)) pats
  , deconstructions
  )
  where
  pats | maxPatternSize > 0  =  take maxPatternSize $ conjurePats maxPatternDepth es nm f
       | otherwise           =                        conjurePats maxPatternDepth es nm f
  fss  =  concatMapT ps2fss pats
  -- replaces the any guard symbol with a guard of the correct type
  es  =  [if isGuardSymbol e then conjureGuard f else e | (e,_) <- ps]

  eh  =  holeAsTypeOf efxs
  efxs  =  conjureVarApplication nm f
  (ef:_)  =  unfoldApp efxs

  unguardT | any isGuardSymbol es  =  discardT isGuard
           | otherwise             =  id

  keep | rewriting  =  isRootNormalC thy . fastMostGeneralVariation
       | otherwise  =  const True

  keepBndn | rewriting  =  \b@(_,rhs) -> isBaseCase b || size (normalize thy rhs) >= size rhs
           | otherwise  =  const True

  appsWith :: Expr -> [Expr] -> [[Expr]]
  appsWith eh vs  =  enumerateAppsFor eh k $ vs ++ es
    where
    k e  =  keepNumeric e && keepConstant e && keep e
    -- discards non-atomic numeric ground expressions such as 1 + 1
    keepNumeric | atomicNumbers && isNumeric eh  =  \e -> isFun e || isConst e || not (isGround e)
                | otherwise                      =  const True
    -- discards big non-atomic ground expressions such as 1 + 1 or reverse [1,2]
    keepConstant | maxConstantSize > 0  =  \e -> isFun e || isConst e || not (isGround e) || size e <= maxConstantSize
                 | otherwise            =  const True

  isRedundant | adHocRedundancy  =  \e -> isRedundantDefn e || isRedundantModuloRewriting (normalize thy) e
              | otherwise        =  const False

  hasRedundant | adHocRedundancy  =  hasRedundantRecursion
               | otherwise        =  const False

  isNumeric  =  conjureIsNumeric f

  (-==-)  =  conjureMkEquation f
  tests  =  conjureTestDefn maxTests maxSearchTests nm f
  exprExpr  =  conjureExpress f

  ps2fss :: [Expr] -> [[Defn]]
  ps2fss pats  =  discardT isRedundant
               .  products  -- alt: use delayedProducts
               $  map p2eess pats
    -- delayedProducts makes the number of patterns counts as the size+1.
    where
    p2eess :: Expr -> [[Bndn]]
    -- the following guarded line is an optional optimization
    -- if the function is defined for the given pattern,
    -- simply use its return value as the only possible result
    p2eess pat | copyBindings && isGroundPat f pat  =  [[(pat, toValPat f pat)]]
    p2eess pat  =  mapT (pat,)
                .  filterT keepBase
                .  appsWith pat
                .  drop 1 -- this excludes the function name itself
                $  vars pat ++ [eh | any (uncurry should) (zip aess aes)]
      where
      keepBase
        | not earlyTests  =  const True
        | all isVar (unfoldApp pat)  =  const True
        | otherwise  =  \e -> hasHole e || reallyKeepBase e
      reallyKeepBase e  =  and
        [ errorToFalse $ eval False $ (e //- bs) -==- rhs
        | (lhs,rhs) <- tests
        -- filter test bindings that match the current pattern:
        , Just bs <- [lhs `matchArgs` pat]
        ]
      matchArgs efxs efys  =  fold (map exprExpr (drop 1 (unfoldApp efxs)))
                      `match` fold               (drop 1 (unfoldApp efys))

      -- computes whether we should include a recurse for this given argument
      -- numeric arguments additionally require 0 to be present as a case
      -- for recursion
      should aes ae  =  length (nub aes) > 1 && hasVar ae && (isApp ae || isUnbreakable ae)
                     && (not requireZero || not (isNumeric ae) || any isZero aes)
      aes   =                  (tail . unfoldApp . rehole) pat
      aess  =  transpose $ map (tail . unfoldApp . rehole) pats

  fillingsFor1 :: Bndn -> [[Bndn]]
  fillingsFor1 (ep,er)  =  filterT keepBndn
                        .  mapT (\es -> (ep,fill er es))
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
                   $  unguardT
                   $  concatMapT (`appsWith` hs) [hs]
    where
    hs  =  nub $ conjureArgumentHoles f

  recs :: Expr -> [[Expr]]
  recs ep  =  filterT (keepR ep)
           .  discardT (\e -> e == ep)
           $  recsV' (tail (vars ep))

  recsV :: [Expr] -> [[Expr]]
  recsV vs  =  filterT (\e -> any (`elem` vs) (vars e))
            $  foldAppProducts ef [unguardT $ appsWith h vs | h <- conjureArgumentHoles f]

  -- like recs, but memoized
  recs' :: Expr -> [[Expr]]
  recs' ep  =  fromMaybe errRP $ lookup ep eprs
    where
    eprs = [(ep, recs ep) | ep <- concat possiblePats]

  possiblePats :: [[Expr]]
  possiblePats  =  map (nubSort . concat) $ pats

  -- like recsV, but memoized
  recsV' :: [Expr] -> [[Expr]]
  recsV' vs  =  fromMaybe errRV $ lookup (nubSort vs) evrs
    where
    evrs = [(vs, recsV vs) | vs <- concatMap nubSort $ mapT (nubSort . tail . vars) possiblePats]

  errRP  =  error "candidateDefnsC: unexpected pattern.  You have found a bug, please report it."
  errRV  =  error "candidateDefnsC: unexpected variables.  You have found a bug, please report it."

  thy  =  doubleCheck (===)
       .  theoryFromAtoms (===) maxEquationSize . (:[]) . nub
       $  cjHoles (fun nm f:ps) ++ [val False, val True] ++ es
  (===)  =  cjAreEqual (fun nm f:ps) maxTests
  isUnbreakable  =  conjureIsUnbreakable f


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

delayedProducts :: [ [[a]] ] -> [[ [a] ]]
delayedProducts xsss  =  products xsss `addWeight` (length xsss - 1)
-- TODO: move delayedProducts to LeanCheck?

delayedProductsWith :: ([a] -> a) -> [ [[a]] ] -> [[a]]
delayedProductsWith f xsss  =  productsWith f xsss `addWeight` length xsss
-- TODO: move delayedProductsWith to LeanCheck?

foldAppProducts :: Expr -> [ [[Expr]] ] -> [[Expr]]
foldAppProducts ef  =  delayedProductsWith (foldApp . (ef:))

-- show time in seconds rounded to one decimal place
-- the argument is expected to be in picoseconds
showTime :: Integer -> String
showTime ps  =  show s ++ "s"
  where
  s  =  fromIntegral ds / 10.0 -- seconds
  ds  =  ps `div` 100000000000 -- deciseconds, * 10 / 1 000 000 000 000

-- beware of lazyness, this computes the time for evaluating msg!
putWithTimeSince :: Integer -> String -> IO ()
putWithTimeSince start msg
  | start < 0   =  putStrLn $ "-- " ++ msg  -- negative start time indicates omit runtime
  | msg == msg  =  do  -- forces evaluation of msg!
                   end <- getCPUTime
                   putStrLn $ "-- " ++ showTime (end - start) ++ ", " ++ msg
  | otherwise   =  error "putWithTimeSince: the impossible happened (GHC/Compiler/Interpreter bug?!)"

-- consume tiers until a target is reached, then stop
targetiers :: Int -> [[a]] -> [[a]]
targetiers n xss
  | n <= 0     =  []
  | otherwise  =  case xss of [] -> []
                              (xs:xss) -> xs : targetiers (n - length xs) xss

boolTy :: TypeRep
boolTy  =  typ b_
