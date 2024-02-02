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

  , rehole
  , funToVar
  , recursexpr
  , apparentlyTerminates
  , mayNotEvaluateArgument
  , compareSimplicity
  , ifFor
  , caseForOrd
  , valuesBFS
  , holesBFS
  , fillBFS
  , ($$**)
  , ($$|<)
  , possibleHoles
  , revaluate
  , reval
  , useMatches
  , deholings
  , varToConst
  , hasAppInstanceOf
  , isNegative

  , enumerateAppsFor
  , enumerateFillings

  , digApp
  , extractApp
  , updateAppAt
  , ($!!)

  , conflicts
  , listConflicts

  , grounds
  , groundBinds

  , module Conjure.Utils
  )
where

import Conjure.Utils

import Data.Express
import Data.Express.Utils.Typeable
import Data.Express.Fixtures hiding ((-==-))

import Data.Dynamic
import Control.Applicative ((<$>)) -- for GHC <= 7.8

import Test.LeanCheck (mapT, filterT, (\/), delay, productWith, productMaybeWith)
import Test.LeanCheck.Tiers (products)
import Test.LeanCheck.Utils.Types (A, B, C, D, E, F)

import Test.Speculate.Expr (grounds, groundBinds)

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

-- | Given a variable, returns a constant with the same name
varToConst :: Expr -> Expr
varToConst (Value ('_':nm) dyn)  =  Value nm dyn
varToConst _  =  error "varToConst: can only be applied to variables"

-- | Returns whether the first 'Expr'
--   has an application instance of the second 'Expr'.
hasAppInstanceOf :: Expr -> Expr -> Bool
e `hasAppInstanceOf` efxs  =  constApp e `hasInstanceOf` constApp efxs
  where
  constApp e  =  e //- [(ef,varToConst ef)]
  (ef:_)  =  unfoldApp efxs

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

-- | Creates an if 'Expr' of the type of the given proxy.
--
-- > > ifFor (undefined :: Int)
-- > if :: Bool -> Int -> Int -> Int
--
-- > > ifFor (undefined :: String)
-- > if :: Bool -> [Char] -> [Char] -> [Char]
ifFor :: Typeable a => a -> Expr
ifFor a  =  value "if" (\p x y -> if p then x else y `asTypeOf` a)

-- | Creates a case 'Expr' of the type of the given proxy.
--
-- > > caseForOrd (undefined :: Int)
-- > case :: Ordering -> Int -> Int -> Int -> Int
--
-- > > caseForOrd (undefined :: String)
-- > case :: Ordering -> [Char] -> [Char] -> [Char] -> [Char]
caseForOrd :: Typeable a => a -> Expr
caseForOrd a  =  value "case" (\o x y z -> case o of LT -> x; EQ -> y; GT -> z `asTypeOf` a)

-- | Lists terminal values in BFS order.
--
-- (cf. 'values', 'holesBFS', 'fillBFS')
valuesBFS :: Expr -> [Expr]
valuesBFS  =  concat . bfs
  where
  bfs :: Expr -> [[Expr]]
  bfs (ef :$ ex)  =  [] : mzip (bfs ef) (bfs ex)
  bfs e  =  [[e]]

-- | Lists holes in BFS order.
--
-- (cf. 'holes', 'valuesBFS', 'fillBFS')
holesBFS :: Expr -> [Expr]
holesBFS  =  filter isHole . valuesBFS

-- | Fills holes in BFS order.
--
-- (cf. 'fill', 'valuesBFS', 'fillBFS')
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

-- | Like '$$' but always works regardless of type.
--
-- /Warning:/ just like ':$', this may produce ill-typed expressions.
--
-- > > zero $$** zero
-- > Just (0 0 :: ill-typed # Int $ Int #)
--
-- Together with '$$|<', this function is unused
-- but is useful when experiment with the source
-- to see the effect of type-corrected
-- on pruning the search space.
--
-- (cf. '$$', '$$|<')
($$**) :: Expr -> Expr -> Maybe Expr
e1 $$** e2  =  Just $ e1 :$ e2

-- | Like '$$' but relaxed to work on correct kinds.
--
-- > > ordE $$|< zero
-- > Just (ord 0 :: ill-typed # Char -> Int $ Int #)
--
-- > > zero $$|< zero
-- > Nothing
--
-- /Warning:/ just like ':$', this may produce ill-typed expressions.
--
-- Together with '$$**', this function is unused
-- but is useful when experiment with the source
-- to see the effect of type-corrected
-- on pruning the search space.
--
-- (cf. '$$', '$$**')
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


-- | Lists all distinct holes that are possible with the given 'Expr's.
--
-- > > possibleHoles [zero, one, plus]
-- > [_ :: Int,_ :: Int -> Int,_ :: Int -> Int -> Int]
--
-- > > possibleHoles [ae, ordE]
-- > [_ :: Char,_ :: Int,_ :: Char -> Int]
possibleHoles :: [Expr] -> [Expr]
possibleHoles  =  nubSort . ph . nubSort . map holeAsTypeOf
  where
  ph hs  =  case nubSort $ hs ++ [holeAsTypeOf hfx | hf <- hs, hx <- hs, Just hfx <- [hf $$ hx]] of
            hs' | hs' == hs -> hs
                | otherwise -> ph hs'


-- -- Expression enumeration -- --

-- | Enumerate applications between values of the given list of primitives
--   and of the given expressions's type.
--
-- __Arguments:__
--
-- 1. an 'Expr' whose type we are interested in
-- 2. a filtering function, returning 'True' for the expressions to keep
-- 3. a list of primitives to be used in building expression.
--
-- __Result:__ a potentially infinite list of list of enumerated expressions
--
-- The enumeration here is type-directed for performance reasons.
enumerateAppsFor :: Expr -> (Expr -> Bool) -> [Expr] -> [[Expr]]
enumerateAppsFor h keep es  =  for h
  where
  hs :: [Expr]
  hs  =  possibleHoles es
  for :: Expr -> [[Expr]]
  for h  =  filter (\e -> typ h == typ e) es : apps
    where
    apps  =  foldr (\/) []
          [  filterT keep $ fliproductWith (:$) (for hf) (for hx)
          |  hf <- hs
          ,  hx <- hs
          ,  Just hfx <- [hf $$ hx]
          ,  typ h == typ hfx
          ]

-- | Given an expression whose holes are /all of the same type/
--   and an enumeration of 'Expr's of this same type,
--   enumerate all possible fillings of the original expression
--   with the 'Expr's in the enumeration.
enumerateFillings :: Expr -> [[Expr]] -> [[Expr]]
enumerateFillings e  =  mapT (fill e)
                     .  products
                     .  replicate (length $ holes e)

-- | Evaluates an 'Expr' to a 'Dynamic' value
--   using the given recursive definition and
--   maximum number of recursive calls.
--
-- (cf. 'Conjure.Defn.toDynamicWithDefn')
recursiveToDynamic :: (Expr,Expr) -> Int -> Expr -> Maybe Dynamic
recursiveToDynamic (efxs, ebody) n  =  fmap (\(_,_,d) -> d) . re (n * size ebody) n
  where
  (ef':exs')  =  unfoldApp efxs

  rev :: Typeable a => Int -> Int -> Expr -> Maybe (Int, Int, a)
  rev m n e  =  case re m n e of
                Nothing    -> Nothing
                Just (m,n,d) -> case fromDynamic d of
                                Nothing -> Nothing
                                Just x  -> Just (m, n, x)

  re :: Int -> Int -> Expr -> Maybe (Int, Int, Dynamic)
  re m n _  | n <= 0  =  error "recursiveToDynamic: recursion limit reached"
  re m n _  | m <= 0  =  error "recursiveToDynamic: evaluation limit reached"
  re m n (Value "if" _ :$ ec :$ ex :$ ey)  =  case rev m n ec of
    Nothing    -> Nothing
    Just (m,n,True)  -> re m n ex
    Just (m,n,False) -> re m n ey
  re m n (Value "||" _ :$ ep :$ eq)  =  case rev m n ep of
    Nothing        -> Nothing
    Just (m,n,True)  -> (m,n,) <$> toDynamic (val True)
    Just (m,n,False) -> re m n eq
  re m n (Value "&&" _ :$ ep :$ eq)  =  case rev m n ep of
    Nothing    -> Nothing
    Just (m,n,True)  -> re m n eq
    Just (m,n,False) -> (m,n,) <$> toDynamic (val False)
  re m n e  =  case unfoldApp e of
    [] -> error "recursiveToDynamic: empty application unfold"  -- should never happen
    [e] -> (m-1,n,) <$> toDynamic e
    (ef:exs) | ef == ef' -> re m (n-1) $ ebody //- zip exs' exs
             | otherwise -> foldl ($$) (re m n ef) exs

  Just (m,n,d1) $$ e2  =  case re m n e2 of
                          Nothing -> Nothing
                          Just (m', n', d2) -> (m',n',) <$> dynApply d1 d2
  _ $$ _               =  Nothing

-- | Evaluates an 'Expr' to a regular Haskell value
--   using the given recursive definition and
--   maximum number of recursive calls.
--   If there's a type mismatch, this function returns 'Nothing'.
--
-- (cf. 'evaluate', 'Conjure.Defn.devaluate')
revaluate :: Typeable a => (Expr,Expr) -> Int -> Expr -> Maybe a
revaluate dfn n e  =  recursiveToDynamic dfn n e >>= fromDynamic

-- | Evaluates an 'Expr' to a regular Haskell value
--   using the given recursive definition and
--   maximum number of recursive calls.
--   If there's a type mismatch,
--   this function returns the given default value.
--
-- (cf. 'eval', 'Conjure.Defn.deval')
reval :: Typeable a => (Expr,Expr) -> Int -> a -> Expr -> a
reval dfn n x  =  fromMaybe x . revaluate dfn n

-- | like 'productWith' but prefers enumerating from the second tiers first
fliproductWith :: (a->b->c) -> [[a]] -> [[b]] -> [[c]]
fliproductWith _ [] _  =  []
fliproductWith _ _ []  =  []
fliproductWith f xss (ys:yss)  =  map (** ys) xss
                               \/ delay (productWith f xss yss)
  where
  xs ** ys  =  [x `f` y | x <- xs, y <- ys]

-- |
--
-- > useMatches [xx,yy] [xx,yy]  =  [[(xx,xx), (yy,yy)]]
-- > useMatches [xx,yy] [yy,xx]  =  [[(xx,xx), (yy,yy)]]
-- > useMatches [yy,xx] [xx,yy]  =  [[(yy,yy), (xx,xx)]]
-- > useMatches [xx,yy] [xx,xx]  =  []
-- > useMatches [xx,yy] [abs' xx, abs' yy]  =  [[(xx,abs' xx), (yy, abs' yy)]]
-- > useMatches [xx-:-xxs, yy-:-yys] [abs' xx, abs' yy]
-- >   =  [(xx-:-xxs, abs' xx), (yy-:-yys, abs' yy)]
useMatches :: [Expr] -> [Expr] -> [[(Expr,Expr)]]
useMatches [] []  =  [[]]
useMatches [] es  =  [] -- no matches when lists have different lengths
useMatches es []  =  [] -- no matches when lists have different lengths
useMatches (e:es) es'  =  concat
  [ map ((e,e'):) (useMatches es es')
  | (e',es') <- choicesThat (\e' _ -> any (`elem` vars e') (vars e)) es'
  ]

-- | Turns all variables of an expression into holes.
--
-- > > rehole (xx -+- yy)
-- > _ + _ :: Int
rehole :: Expr -> Expr
rehole (e1 :$ e2)    = rehole e1 :$ rehole e2
rehole e | isVar e   = "" `varAsTypeOf` e
         | otherwise = e

-- | Takes two expressions and returns all possible ways
--   in which the first expression can appear once in
--   one of the holes of the second expression.
--
-- > > deholings zero (i_ -+- i_ -+- i_)
-- > [ (0 + _) + _ :: Int
-- > , (_ + 0) + _ :: Int
-- > , (_ + _) + 0 :: Int
-- > ]
--
-- > > deholings zero (i_ -+- one -+- ord' c_)
-- > [(0 + 1) + ord _ :: Int]
deholings :: Expr -> Expr -> [Expr]
deholings e'  =  deh
  where
  deh (e1 :$ e2)  =  map (:$ e2) (deh e1)
                  ++ map (e1 :$) (deh e2)
  deh e  =  if typ e == typ e' && isHole e
            then [e']
            else []

-- | Dig a hole in a function application at the given position
--
-- > > digApp 1 (one -+- two)
-- > _ + 2 :: Int
--
-- > > digApp 2 (one -+- two)
-- > 1 + _ :: Int
digApp :: Int -> Expr -> Expr
digApp i  =  updateAppAt i holeAsTypeOf

updateAppAt :: Int -> (Expr -> Expr) -> Expr -> Expr
updateAppAt i f  =  foldApp . updateAt i f . unfoldApp

-- | Extracts the argument of a function application at the given position.
--
-- > (one -+- two) $!! 1
-- 1 :: Int
--
-- > (one -+- two) $!! 2
-- 2 :: Int
($!!) :: Expr -> Int -> Expr
e $!! i  =  unfoldApp e !! i

-- | Extracts a value in a function application at the given position
--
-- > > extractApp 1 (one -+- two)
-- > (_ + 2 :: Int, 1 :: Int)
--
-- > > extractApp 2 (one -+- two)
-- > (1 + _ :: Int, 2 :: Int)
extractApp :: Int -> Expr -> (Expr,Expr)
extractApp i efxs  =  (foldApp $ updateAt i holeAsTypeOf es, es !! i)
  where
  es  =  unfoldApp efxs


-- | Lists conflicts between two expressions
--
-- > > conflicts (one -+- two) (three -+- four)
-- > [(1 :: Int,3 :: Int), (2 :: Int,4 :: Int)]
--
-- > > conflicts (xx -:- nil) (xx -:- yy -:- yys)
-- > [(nil, yy -:- yys)]
--
-- > > conflicts  (one -:- one -:- nil) (zero -:- zero -:- xx -:- xxs)
-- > [(1 :: Int,0 :: Int),([] :: [Int],x:xs :: [Int])]
conflicts :: Expr -> Expr -> [(Expr,Expr)]
conflicts e1 e2 | typ e1 /= typ e2  =  [(e1,e2)]
conflicts (ef :$ ex) (eg :$ ey)     =  conflicts ef eg +++ conflicts ex ey
conflicts e1 e2                     =  [(e1,e2) | e1 /= e2]

listConflicts :: [Expr] -> [[Expr]]
listConflicts es
  | not (allEqualOn typ es)  =  [es]
  | all isApp es             =  listConflicts [ef | ef :$ _ <- es]
                            +++ listConflicts [ex | _ :$ ex <- es]
  | otherwise                =  [es | not (allEqual es)]
  where
  fun (ef :$ _)  =  ef
  arg (_ :$ ex)  =  ex

-- | Is the expression encoding a negative number.
--
-- This function is sort of a hack.
isNegative :: Expr -> Bool
isNegative (Value ('-':_) _)  =  True
isNegative _  =  False

instance Express A where  expr  =  val
instance Express B where  expr  =  val
instance Express C where  expr  =  val
instance Express D where  expr  =  val
instance Express E where  expr  =  val
instance Express F where  expr  =  val
