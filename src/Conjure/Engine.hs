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
-- > factorial :: Int -> Int
-- > factorial 2  =  2
-- > factorial 3  =  6
-- > factorial 4  =  24
-- >
-- > primitives :: [Prim]
-- > primitives =
-- >   [ pr (0::Int)
-- >   , pr (1::Int)
-- >   , prim "+" ((+) :: Int -> Int -> Int)
-- >   , prim "*" ((*) :: Int -> Int -> Int)
-- >   , prim "-" ((-) :: Int -> Int -> Int)
-- >   ]
--
-- The conjure function does the following:
--
-- > > conjure "factorial" factorial primitives
-- > factorial :: Int -> Int
-- > -- testing 3 combinations of argument values
-- > -- pruning with 27/65 rules
-- > -- ...  ...  ...
-- > -- looking through 185 candidates of size 7
-- > -- tested 107 candidates
-- > factorial 0  =  1
-- > factorial x  =  x * factorial (x - 1)
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
-- > conjureWithMaxSize 18 "function" function [...]
--
-- For example, given the following partial definition for 'Data.List.insert':
--
-- > insert' :: Int -> [Int] -> [Int]
-- > insert' 0 []  =  [0]
-- > insert' 0 [1,2]  =  [0,1,2]
-- > insert' 1 [0,2]  =  [0,1,2]
-- > insert' 2 [0,1]  =  [0,1,2]
--
-- Conjure is able to find an appropriate definition at size 17
-- with the following primitives:
--
-- > > conjureWithMaxSize 18 "insert" insert'
-- > >   [ prim "[]" ([] :: [Int])
-- > >   , prim ":" ((:) :: Int -> [Int] -> [Int])
-- > >   , prim "<=" ((<=) :: Int -> Int -> Bool)
-- > >   , prif (undefined :: [Int])
-- > >   ]
-- > insert :: Int -> [Int] -> [Int]
-- > -- testing 4 combinations of argument values
-- > -- pruning with 4/4 rules
-- > -- ...  ...  ...
-- > -- looking through 14550 candidates of size 17
-- > -- tested 14943 candidates
-- > insert x []  =  [x]
-- > insert x (y:xs)  =  if x <= y
-- >                     then x:insert y xs
-- >                     else y:insert x xs
--
-- The default maximum size of 12 would not be enough for the above definition.
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
  , maxConstantSize       :: Int  -- ^ maximum size of constants (0 for no limit)

  -- advanced & debug options --
  , carryOn               :: Bool -- ^ whether to carry on after finding a suitable candidate
  , showTheory            :: Bool -- ^ show theory discovered by Speculate used in pruning
  , usePatterns           :: Bool -- ^ use pattern matching to create (recursive) candidates
  , showCandidates        :: Bool -- ^ (debug) show candidates -- warning: wall of text
  , showTests             :: Bool -- ^ (debug) show tests
  , showPatterns          :: Bool -- ^ (debug) show possible LHS patterns
  , showDeconstructions   :: Bool -- ^ (debug) show conjectured-and-allowed deconstructions

  -- pruning options --
  , rewriting             :: Bool -- ^ unique-modulo-rewriting candidates
  , requireDescent        :: Bool -- ^ require recursive calls to deconstruct arguments
  , adHocRedundancy       :: Bool -- ^ ad-hoc redundancy checks
  , copyBindings          :: Bool -- ^ copy partial definition bindings in candidates
  , atomicNumbers         :: Bool -- ^ restrict constant/ground numeric expressions to atoms
  , requireZero           :: Bool -- ^ require 0 as base case for Num recursions
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
-- * do not show tested candidates
-- * do not make candidates unique module testing
args :: Args
args = Args
  { maxTests               =  360
  , maxSize                =  12
  , maxEvalRecursions      =  60
  , maxEquationSize        =   5
  , maxSearchTests         =  100000
  , maxDeconstructionSize  =   4
  , maxConstantSize        =   0 -- unlimited

  -- advanced & debug options --
  , carryOn                =  False
  , showTheory             =  False
  , usePatterns            =  True
  , showCandidates         =  False
  , showTests              =  False
  , showDeconstructions    =  False
  , showPatterns           =  False

  -- pruning options --
  , rewriting              =  True
  , requireDescent         =  True
  , adHocRedundancy        =  True
  , copyBindings           =  True
  , atomicNumbers          =  True
  , requireZero            =  False
  , uniqueCandidates       =  False
  }

-- TODO: remove the requireZero option from args?


-- | Like 'conjure' but allows setting options through 'Args'/'args'.
--
-- > conjureWith args{maxSize = 18} "function" function [...]
conjureWith :: Conjurable f => Args -> String -> f -> [Prim] -> IO ()
conjureWith args nm f  =  conjure0With args nm f (const True)

-- | Like 'conjureFromSpec' but allows setting options through 'Args'/'args'.
--
-- > conjureFromSpecWith args{maxSize = 18} "function" spec [...]
conjureFromSpecWith :: Conjurable f => Args -> String -> (f -> Bool) -> [Prim] -> IO ()
conjureFromSpecWith args nm p  =  conjure0With args nm undefined p

-- | Like 'conjure0' but allows setting options through 'Args'/'args'.
conjure0With :: Conjurable f => Args -> String -> f -> (f -> Bool) -> [Prim] -> IO ()
conjure0With args nm f p es  =  do
  print (var (head $ words nm) f)
  when (length ts > 0) $ do
    putStrLn $ "-- testing " ++ show (length ts) ++ " combinations of argument values"
    when (showTests args) $ do
      putStrLn $ "{-"
      putStr $ unlines $ map show ts
      putStrLn $ "-}"
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
  when (showPatterns args) $ do
    putStr   $ unlines $ zipWith (\i -> (("-- allowed patterns of size " ++ show i ++ "\n{-\n") ++) . (++ "-}") . unlines) [1..] $ mapT showDefn $ patternss results
  when (showDeconstructions args) $ do
    putStrLn $ "{- List of allowed deconstructions:"
    putStr   $ unlines $ map show $ deconstructions results
    putStrLn $ "-}"
  pr 1 0 rs
  where
  showEq eq  =  showExpr (fst eq) ++ " == " ++ showExpr (snd eq)
  pr :: Int -> Int -> [([Defn], [Defn])] -> IO ()
  pr n t []  =  do putStrLn $ "-- tested " ++ show t ++ " candidates"
                   putStrLn $ "cannot conjure\n"
  pr n t ((is,cs):rs)  =  do
    let nc  =  length cs
    putStrLn $ "-- looking through " ++ show nc ++ " candidates of size " ++ show n
    when (showCandidates args) $
      putStr $ unlines $ ["{-"] ++ map showDefn cs ++ ["-}"]
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
  , bindings :: [Expr]           -- ^ test bindings used to verify candidates
  , theory :: Thy                -- ^ the underlying theory
  , patternss :: [[Defn]]        -- ^ tiers of allowed patterns
  , deconstructions :: [Expr]    -- ^ the list of allowed deconstructions
  }


-- | Like 'conjure' but in the pure world.
--
-- The most important part of the result are the tiers of implementations
-- however results also include candidates, tests and the underlying theory.
conjpure :: Conjurable f => String -> f -> [Prim] -> Results
conjpure =  conjpureWith args

-- | Like 'conjureFromSpec' but in the pure world.  (cf. 'conjpure')
conjpureFromSpec :: Conjurable f => String -> (f -> Bool) -> [Prim] -> Results
conjpureFromSpec  =  conjpureFromSpecWith args

-- | Like 'conjure0' but in the pure world.  (cf. 'conjpure')
conjpure0 :: Conjurable f => String -> f -> (f -> Bool) -> [Prim] -> Results
conjpure0 =  conjpure0With args

-- | Like 'conjpure' but allows setting options through 'Args' and 'args'.
conjpureWith :: Conjurable f => Args -> String -> f -> [Prim] -> Results
conjpureWith args nm f  =  conjpure0With args nm f (const True)

-- | Like 'conjureFromSpecWith' but in the pure world.  (cf. 'conjpure')
conjpureFromSpecWith :: Conjurable f => Args -> String -> (f -> Bool) -> [Prim] -> Results
conjpureFromSpecWith args nm p  =  conjpure0With args nm undefined p

-- | Like 'conjpure0' but allows setting options through 'Args' and 'args'.
--
-- This is where the actual implementation resides.  The functions
-- 'conjpure', 'conjpureWith', 'conjpureFromSpec', 'conjpureFromSpecWith',
-- 'conjure', 'conjureWith', 'conjureFromSpec', 'conjureFromSpecWith' and
-- 'conjure0' all refer to this.
conjpure0With :: Conjurable f => Args -> String -> f -> (f -> Bool) -> [Prim] -> Results
conjpure0With args@(Args{..}) nm f p es  =  Results
  { implementationss  =  implementationsT
  , candidatess  =  candidatesT
  , bindings  =  tests
  , theory  =  thy
  , patternss  =  patternss
  , deconstructions  =  deconstructions
  }
  where
  tests  =  [ffxx //- bs | bs <- dbss]
  implementationsT  =  filterT implements candidatesT
  implements fx  =  defnApparentlyTerminates fx
                 && requal fx ffxx vffxx
                 && errorToFalse (p (cevl maxEvalRecursions fx))
  candidatesT  =  (if uniqueCandidates then nubCandidates args nm f else id)
               $  take maxSize candidatesTT
  (candidatesTT, thy, patternss, deconstructions)  =  candidateDefns args nm f es
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
  Results {theory = thy}  =  conjpureWith args nm f es


-- | Return apparently unique candidate definitions.
--
-- This function returns a trio:
--
-- 1. tiers of candidate definitions
-- 2. an equational theory
-- 3. a list of allowed deconstructions
candidateDefns :: Conjurable f => Args -> String -> f -> [Prim] -> ([[Defn]], Thy, [[Defn]], [Expr])
candidateDefns args  =  candidateDefns' args
  where
  candidateDefns'  =  if usePatterns args
                      then candidateDefnsC
                      else candidateDefns1


-- | Return apparently unique candidate definitions
--   where there is a single body.
candidateDefns1 :: Conjurable f => Args -> String -> f -> [Prim] -> ([[Defn]], Thy, [[Defn]], [Expr])
candidateDefns1 args nm f ps  =  first4 (mapT toDefn) $ candidateExprs args nm f ps
  where
  efxs  =  conjureVarApplication nm f
  toDefn e  =  [(efxs, e)]
  first4 f (x,y,z,w)  =  (f x, y, z, w)


-- | Return apparently unique candidate bodies.
candidateExprs :: Conjurable f => Args -> String -> f -> [Prim] -> ([[Expr]], Thy, [[Defn]], [Expr])
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
       $  cjHoles (prim nm f:ps) ++ [val False, val True] ++ es
  (===)  =  cjAreEqual (prim nm f:ps) maxTests


-- | Return apparently unique candidate definitions
--   using pattern matching.
candidateDefnsC :: Conjurable f => Args -> String -> f -> [Prim] -> ([[Defn]], Thy, [[Defn]], [Expr])
candidateDefnsC Args{..} nm f ps  =
  ( discardT hasRedundant $ concatMapT fillingsFor fss
  , thy
  , mapT (map (,eh)) pats
  , deconstructions
  )
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
    k | atomicNumbers && isNumeric eh  =  \e -> keepNumeric e && keep e
      | maxConstantSize > 0            =  \e -> keepConstant e && keep e
      | otherwise                      =  keep
    -- discards non-atomic numeric ground expressions such as 1 + 1
    keepNumeric e  =  isFun e || isConst e || not (isGround e)
    -- discards big non-atomic ground expressions such as 1 + 1 or reverse [1,2]
    keepConstant e  =  isFun e || isConst e || not (isGround e) || size e <= maxConstantSize

  isRedundant | adHocRedundancy  =  \e -> isRedundantDefn e || isRedundantModuloRewriting (normalize thy) e
              | otherwise        =  const False

  hasRedundant | adHocRedundancy  =  hasRedundantRecursion
               | otherwise        =  const False

  isNumeric  =  conjureIsNumeric f

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
                .  appsWith pat
                .  drop 1 -- this excludes the function name itself
                $  vars pat ++ [eh | any (uncurry should) (zip aess aes)]
      where
      -- computes whether we should include a recurse for this given argument
      -- numeric arguments additionally require 0 to be present as a case
      -- for recursion
      should aes ae  =  length (nub aes) > 1 && hasVar ae && (isApp ae || isUnbreakable ae)
                     && (not requireZero || not (isNumeric ae) || any isZero aes)
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

delayedProducts :: [ [[a]] ] -> [[ [a] ]]
delayedProducts xsss  =  products xsss `addWeight` (length xsss - 1)
-- TODO: move delayedProducts to LeanCheck?

delayedProductsWith :: ([a] -> a) -> [ [[a]] ] -> [[a]]
delayedProductsWith f xsss  =  productsWith f xsss `addWeight` length xsss
-- TODO: move delayedProductsWith to LeanCheck?

foldAppProducts :: Expr -> [ [[Expr]] ] -> [[Expr]]
foldAppProducts ef  =  delayedProductsWith (foldApp . (ef:))

boolTy :: TypeRep
boolTy  =  typ b_
