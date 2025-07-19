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
  , conjureFromSpec
  , conjure0
  , Results(..)
  , conjpure
  , conjpureFromSpec
  , conjpure0
  , candidateExprs
  , candidateDefns

  -- * settings
  , maxTests
  , maxSize
  , target

  -- * Advanced settings
  , maxRecursions
  , maxEquationSize
  , maxSearchTests
  , maxDeconstructionSize
  , maxConstantSize
  , maxPatternSize
  , maxPatternDepth

  -- * Debug options
  , showCandidates
  , showTheory
  , singlePattern
  , showTests
  , showPatterns
  , showDeconstructions
  , carryOn

  -- * Pruning options
  , dontRewrite
  , dontRequireDescent
  , omitAssortedPruning
  , omitEarlyTests
  , dontCopyBindings
  , nonAtomicNumbers
  , uniqueCandidates

  -- * Properties
  , Property
  , property

  -- * other modules
  , module Data.Express
  , module Data.Express.Fixtures
  , module Conjure.Reason
  , module Conjure.Ingredient
  )
where

import Control.Monad (when)

import Data.Express
import Data.Express.Fixtures hiding ((-==-))

import Test.LeanCheck
import Test.LeanCheck.Tiers
import Test.LeanCheck.Error (errorToFalse, errorToLeft)

import Conjure.Expr
import Conjure.Conjurable
import Conjure.Ingredient
import Conjure.Defn
import Conjure.Defn.Redundancy
import Conjure.Defn.Test
import Conjure.Red
import Conjure.Reason
import Conjure.Settings

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
-- >   [ unfun (0::Int)
-- >   , unfun (1::Int)
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
-- The ingredients list is defined with 'unfun' and 'fun'.
conjure :: Conjurable f => String -> f -> [Ingredient] -> IO ()
conjure nm f  =  conjure0 nm f (const [])


-- | Conjures an implementation from a function specification.
--
-- This function works like 'conjure' but instead of receiving a partial definition
-- it receives a collection of test properties about the function.
--
-- For example, given:
--
-- > squarePropertySpec :: (Int -> Int) -> [Property]
-- > squarePropertySpec square  =
-- >   [ property $ \x -> square x >= x
-- >   , property $ \x -> square x >= 0
-- >   , property $ square 2 == 4
-- >   ]
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
conjureFromSpec :: Conjurable f => String -> (f -> [Property]) -> [Ingredient] -> IO ()
conjureFromSpec nm p  =  conjure0 nm undefined p


-- | Synthesizes an implementation from both a partial definition and a
--   function specification.
--
--   This works like the functions 'conjure' and 'conjureFromSpec' combined.
conjure0 :: Conjurable f => String -> f -> (f -> [Property]) -> [Ingredient] -> IO ()
conjure0 nm f p es  =  do
  -- the code section below became quite ugly with time and patches.
  -- it is still maintainable and readable as it is, but perhaps
  -- needs to be cleaned up and simplified
  t0 <- getCPUTime
  print (var (head $ words nm) f)
  when (length ts > 0) $ do
    putWithTimeSince t0 $ "testing " ++ show (length ts) ++ " combinations of argument values"
    when showTests $ do
      putStrLn $ "{-"
      putStr $ showDefn ts
      putStrLn $ "-}"
  if length ts == 0 && p undefined == []
  then putStrLn $ nm ++ "  =  error \"could not reify specification, suggestion: conjureFromSpec\"\n"
  else do
    putWithTimeSince t0 $ "pruning with " ++ show nRules ++ "/" ++ show nREs ++ " rules"
    when showTheory $ do
      putStrLn $ "{-"
      printThy thy
      putStrLn $ "-}"
    when (not . null $ invalid thy) $ do
      putStrLn $ "-- reasoning produced "
              ++ show (length (invalid thy)) ++ " incorrect properties,"
              ++ " please re-run with more tests for faster results"
      when showTheory $ do
        putStrLn $ "{-"
        putStrLn $ "invalid:"
        putStr   $ unlines $ map showEq $ invalid thy
        putStrLn $ "-}"
    when showPatterns $ do
      putStr $ unlines
             $ zipWith (\i -> (("-- allowed patterns of size " ++ show i ++ "\n{-\n") ++) . (++ "-}") . unlines) [1..]
             $ mapT showDefn
             $ patternss results
    when showDeconstructions $ do
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
    when showCandidates $
      putStr $ unlines $ ["{-"] ++ map showDefn cs ++ ["-}"]
    case is of
      []     ->  pr t0 (n+1) (t+nc) rs
      (_:_)  ->  do pr1 t is cs
                    when carryOn $ pr t0 (n+1) (t+nc) rs
    where
    pr1 t [] cs  =  return ()
    pr1 t (i:is) cs  =  do
      let (cs',cs'') = break (i==) cs
      let t' = t + length cs' + 1
      putWithTimeSince t0 $ "tested " ++ show t' ++ " candidates"
      putStrLn $ showDefn $ etaReduce $ normalizeDefn thy i
      when carryOn $ pr1 t' is (drop 1 cs'')
  rs  =  zip iss css
  results  =  conjpure0 nm f p es
  iss  =  implementationss results
  css  =  candidatess results
  ts   =  bindings results
  thy  =  theory results
  nRules  =  length (rules thy)
  nREs  =  length (equations thy) + nRules
  -- we could avoid the following as most are called once
  -- but is nice to have a summary of which settings are used
  carryOn              =  carryOnI es
  showTests            =  showTestsI es
  showTheory           =  showTheoryI es
  showPatterns         =  showPatternsI es
  showCandidates       =  showCandidatesI es
  showDeconstructions  =  showDeconstructionsI es


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
conjpure nm f  =  conjpure0 nm f (const [])

-- | Like 'conjureFromSpec' but in the pure world.  (cf. 'conjpure')
conjpureFromSpec :: Conjurable f => String -> (f -> [Property]) -> [Ingredient] -> Results
conjpureFromSpec nm p  =  conjpure0 nm undefined p

-- | This is where the actual implementation resides.
-- The functions
-- 'conjpure', 'conjpureFromSpec', 'conjure' and 'conjureFromSpec'
-- all refer to this.
conjpure0 :: Conjurable f => String -> f -> (f -> [Property]) -> [Ingredient] -> Results
conjpure0 nm f p es  =  Results
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
                 && errorToFalse (testSpec maxTests $ p (cevl maxRecursions fx))
  candidatesT  =  (if uniqueCandidates then nubCandidates maxTests maxRecursions nm f else id)
               $  (if target > 0 then targetiers target else id)
               $  (if maxSize > 0 then take maxSize else id)
               $  candidatesTT
  (candidatesTT, thy, patternss, deconstructions)  =  candidateDefns nm f es

  test dfn  =  all (errorToFalse . deval (conjureExpress f) maxRecursions dfn False)
            $  [funToVar lhs -==- rhs | (lhs, rhs) <- tests]
  tests  =  conjureTestDefn maxTests maxSearchTests nm f
  (-==-)  =  conjureMkEquation f
  maxTests  =  maxTestsI es
  (target, maxSize)  =  targetAndMaxSizeI es
  maxRecursions  =  maxRecursionsI es
  maxSearchTests  =  maxSearchTestsI es
  uniqueCandidates  =  uniqueCandidatesI es


-- | Return apparently unique candidate definitions.
--
-- This function returns a trio:
--
-- 1. tiers of candidate definitions
-- 2. an equational theory
-- 3. a list of allowed deconstructions
candidateDefns :: Conjurable f => String -> f -> [Ingredient] -> ([[Defn]], Thy, [[Defn]], [Expr])
candidateDefns nm f is  =  candidateDefns' nm f is
  where
  candidateDefns'  =  if singlePatternI is
                      then candidateDefns1
                      else candidateDefnsC


-- | Return apparently unique candidate definitions
--   where there is a single body.
candidateDefns1 :: Conjurable f => String -> f -> [Ingredient] -> ([[Defn]], Thy, [[Defn]], [Expr])
candidateDefns1 nm f ps  =  first4 (mapT toDefn) $ candidateExprs nm f ps
  where
  efxs  =  conjureVarApplication nm f
  toDefn e  =  [(efxs, e)]
  first4 f (x,y,z,w)  =  (f x, y, z, w)


-- | Return apparently unique candidate bodies.
candidateExprs :: Conjurable f => String -> f -> [Ingredient] -> ([[Expr]], Thy, [[Defn]], [Expr])
candidateExprs nm f is  =
  ( as \/ concatMapT (`enumerateFillings` recs) ts
  , thy
  , [[ [(efxs, eh)] ]]
  , deconstructions
  )
  where
  ps  =  actual is  -- extract actual primitives
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
  keep | rewrite    =  isRootNormalC thy . fastMostGeneralVariation
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
  maxTests               =  maxTestsI is
  maxEquationSize        =  maxEquationSizeI is
  maxDeconstructionSize  =  maxDeconstructionSizeI is
  requireDescent         =  requireDescentI is
  rewrite                =  rewriteI is


-- | Return apparently unique candidate definitions
--   using pattern matching.
candidateDefnsC :: Conjurable f => String -> f -> [Ingredient] -> ([[Defn]], Thy, [[Defn]], [Expr])
candidateDefnsC nm f is =
  ( discardT hasRedundant $ concatMapT fillingsFor partialDefns
  , thy
  , mapT (map (,eh)) pats
  , deconstructions
  )
  where
  pats | maxPatternSize > 0  =  take maxPatternSize $ conjurePats maxPatternDepth es nm f
       | otherwise           =                        conjurePats maxPatternDepth es nm f
  partialDefns  =  concatMapT partialDefnsFromPats pats
  -- replaces the any guard symbol with a guard of the correct type
  ps  =  actual is  -- extract actual ingredients/primitives from the list
  es  =  [if isGuardSymbol e then conjureGuard f else e | (e,_) <- ps]

  eh  =  conjureResultHole f
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
  etests  =  map (first (mappArgs exprExpr)) tests
  tests  =  conjureTestDefn maxTests maxSearchTests nm f
  exprExpr  =  conjureExpress f

  distritests :: [(Expr,Expr)] -> [Expr] -> [([(Expr,Expr)],Expr)]
  distritests _ []  =  []
  distritests ts (pat:pats)  =  (ys, pat) : distritests ns pats
    where
    (ys, ns)  =  partition (\(lhs,_) -> lhs `isInstanceOf` pat) ts

  partialDefnsFromPats :: [Expr] -> [[Defn]]
  partialDefnsFromPats pats  =  discardT isRedundant
                             .  products  -- alt: use delayedProducts
                             .  map (uncurry bindingsForPattern)
                             .  distritests etests
                             $  pats
    -- delayedProducts makes the number of patterns counts as the size+1.
    where
    bindingsForPattern :: [(Expr,Expr)] -> Expr -> [[Bndn]]
    -- the following guarded line is an optional optimization
    -- if the function is defined for the given pattern,
    -- simply use its return value as the only possible result
    bindingsForPattern ts pat
      | copyBindings && isGroundPat f pat  =  [[(pat, toValPat f pat)]]
      | otherwise  =  mapT (pat,)
                   .  filterT keepB
                   .  appsWith pat
                   .  drop 1 -- this excludes the function name itself
                   $  vars pat ++ [eh | any (uncurry should) (zip aess aes)]
      where
      keepB
        | not earlyTests  =  const True
        | length pats < 2  =  const True  -- just one pat, test later
        | otherwise  =  \e -> isNumeric eh && hasHole e || reallyKeepB e
      reallyKeepB e  =  and
        [ errholeToTrue $ eval False $ (e //- bs) -==- rhs
        | (lhs,rhs) <- take 12 ts -- TODO: remove magic number
        -- filter test bindings that match the current pattern:
        , Just bs <- [lhs `match` pat]
        ]

      -- computes whether we should include a recurse for this given argument:
      -- 1. more than one LHS pattern overall
      -- 2. there should be at least a variable
      -- 3. it should either:
      --    * be a breakdown such as _:_ or Tree _ _ _
      --    * or be of an unbreakable/atomic type such as (_ :: Int)
      --      in the presence of deconstructions
      should aes ae  =  length (nub aes) > 1
                     && hasVar ae
                     && (isApp ae || isUnbreakable ae && notNull deconstructions)
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

  maxTests               =  maxTestsI is
  maxSearchTests         =  maxSearchTestsI is
  maxEquationSize        =  maxEquationSizeI is
  maxConstantSize        =  maxConstantSizeI is
  maxDeconstructionSize  =  maxDeconstructionSizeI is
  maxPatternDepth        =  maxPatternDepthI is
  maxPatternSize         =  maxPatternSizeI is
  requireDescent         =  requireDescentI is
  earlyTests             =  earlyTestsI is
  copyBindings           =  copyBindingsI is
  adHocRedundancy        =  assortedPruningI is -- TODO: rename
  atomicNumbers          =  atomicNumbersI is
  rewriting              =  rewriteI is


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

nubCandidates :: Conjurable f => Int -> Int -> String -> f -> [[Defn]] -> [[Defn]]
nubCandidates maxTests maxRecursions nm f  =
  discardLaterT $ equalModuloTesting maxTests maxRecursions nm f


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

normalizeDefn :: Thy -> Defn -> Defn
normalizeDefn  =  map . normalizeBndn

-- This function is quite expensive to run with bad complexity,
-- but is fine on Conjure: candidates are at most a few dozen symbols long
-- and this function just runs once.
normalizeBndn :: Thy -> Bndn -> Bndn
normalizeBndn thy (lhs, rhs)
  | ef `elem` vars rhs  =  (lhs, mapInnerFirstOuterLast commutsort rhs)
  | otherwise           =  (lhs, rhs)
  where
  ef:_  =  unfoldApp lhs
  -- recursive calls come later in this ordering
  commutsort (eo :$ ex :$ ey)
    | isCommutative thy eo  =  if (ef `elem` vars ex, ex)
                               <= (ef `elem` vars ey, ey)
                               then eo :$ ex :$ ey
                               else eo :$ ey :$ ex
  commutsort e  =  e

boolTy :: TypeRep
boolTy  =  typ b_

-- TODO: move these property things to a module of their own?

-- | A test property provided as part of a specification for 'conjureFromSpec'.
--
-- Construct with 'property'.
type Property  =  [Bool]

-- | Provides a single test property to 'conjureFromSpec'.
property :: Testable a => a -> Property
property  =  map snd . results

testSpec :: Int -> [Property] -> Bool
testSpec maxTests  =  and . map (and . take maxTests)

-- like errorToFalse, but returns True upon finding a placeholder hole
errholeToTrue :: Bool -> Bool
errholeToTrue p  =  case errorToLeft p of
                    Right q -> q
                    Left "conjureResultHole: placeholder for recursive call?" -> True
                    Left _ -> False
