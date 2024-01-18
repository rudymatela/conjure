-- |
-- Module      : Conjure.Conjurable
-- Copyright   : (c) 2021 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- This module is part of "Conjure".
--
-- This defines the 'Conjurable' typeclass
-- and utilities involving it.
--
-- You are probably better off importing "Conjure".
module Conjure.Conjurable
  ( Reification1
  , Reification
  , Conjurable (..)
  , conjureType
  , reifyTiers
  , reifyEquality
  , reifyExpress
  , conjureApplication
  , conjureVarApplication
  , conjurePats
  , conjureHoles
  , conjureTiersFor
  , conjureAreEqual
  , conjureMkEquation
  , A, B, C, D, E, F
  , conjureIsDeconstruction
  , candidateDeconstructionsFrom
  , candidateDeconstructionsFromHoled
  , conjureIsUnbreakable
  , conjureReification
  , conjureReification1
  , conjureDynamicEq
  , cevaluate
  , ceval
  , cevl
  , Name (..)
  , Express (..)
  , conjureArgumentPats
  , conjureMostGeneralCanonicalVariation
  )
where

import Test.LeanCheck
import Test.LeanCheck.Utils
import Test.LeanCheck.Error (errorToFalse)
import Conjure.Expr hiding (application)
import Conjure.Defn
import Test.Speculate.Expr
import Data.Functor ((<$>))
import Control.Applicative ((<*>))
import Data.Dynamic
import Data.Express

import Data.Int     -- for instances
import Data.Word    -- for instances
import Data.Ratio   -- for instance
import Data.Complex -- for instance


-- | Single reification of some functions over a type as 'Expr's.
--
-- This is a sixtuple, in order:
--
-- 1. a hole encoded as an 'Expr';
-- 2. the '==' function encoded as an 'Expr' when available;
-- 3. 'tiers' of enumerated test values encoded as 'Expr's when available;
-- 4. infinite list of potential variable names;
-- 5. boolean indicating whether the type is atomic;
-- 6. the 'conjureSize' function encoded as an 'Expr'.
type Reification1  =  (Expr, Maybe Expr, Maybe [[Expr]], [String], Bool, Expr)

-- | A reification over a collection of types.
--
-- Represented as a transformation of a list to a list.
type Reification  =  [Reification1] -> [Reification1]


-- | A primtive expression (paired with instance reification).
type Prim  =  (Expr, Reification)


-- | Provides a primitive value to Conjure.
--   To be used on 'Show' instances.
--   (cf. 'prim')
pr :: (Conjurable a, Show a) => a -> Prim
pr x  =  (val x, conjureType x)


-- | Provides a primitive value to Conjure.
--   To be used on values that are not 'Show' instances
--   such as functions.
--   (cf. 'pr')
prim :: Conjurable a => String -> a -> Prim
prim s x  =  (value s x, conjureType x)


-- | Class of 'Conjurable' types.
-- Functions are 'Conjurable'
-- if all their arguments are 'Conjurable', 'Listable' and 'Show'able.
--
-- For atomic types that are 'Listable',
-- instances are defined as:
--
-- > instance Conjurable Atomic where
-- >   conjureTiers  =  reifyTiers
--
-- For atomic types that are both 'Listable' and 'Eq',
-- instances are defined as:
--
-- > instance Conjurable Atomic where
-- >   conjureTiers     =  reifyTiers
-- >   conjureEquality  =  reifyEquality
--
-- For types with subtypes,
-- instances are defined as:
--
-- > instance Conjurable Composite where
-- >   conjureTiers     =  reifyTiers
-- >   conjureEquality  =  reifyEquality
-- >   conjureSubTypes x  =  conjureType y
-- >                      .  conjureType z
-- >                      .  conjureType w
-- >     where
-- >     (Composite ... y ... z ... w ...)  =  x
--
-- Above @x@, @y@, @z@ and @w@ are just proxies.
-- The @Proxy@ type was avoided for backwards compatibility.
--
-- Please see the source code of "Conjure.Conjurable" for more examples.
--
-- (cf. 'reifyTiers', 'reifyEquality', 'conjureType')
class (Typeable a, Name a) => Conjurable a where
  conjureArgumentHoles :: a -> [Expr]
  conjureArgumentHoles _  =  []

  -- | Returns 'Just' the '==' function encoded as an 'Expr' when available
  --   or 'Nothing' otherwise.
  --
  -- Use 'reifyEquality' when defining this.
  conjureEquality :: a -> Maybe Expr
  conjureEquality _  =  Nothing

  -- | Returns 'Just' 'tiers' of values encoded as 'Expr's when possible
  --   or 'Nothing' otherwise.
  --
  -- Use 'reifyTiers' when defining this.
  conjureTiers :: a -> Maybe [[Expr]]
  conjureTiers _  =  Nothing

  conjureSubTypes :: a -> Reification
  conjureSubTypes _  =  id

  -- | Returns an if-function encoded as an 'Expr'.
  conjureIf :: a -> Expr
  conjureIf   =  ifFor

  -- | Returns a top-level case breakdown.
  conjureCases :: a -> [Expr]
  conjureCases _  =  []

  conjureArgumentCases :: a -> [[Expr]]
  conjureArgumentCases _  =  []

  -- | Returns the (recursive) size of the given value.
  conjureSize :: a -> Int
  conjureSize _  =  0

  -- | Returns a function that deeply reencodes an expression when possible.
  --   ('id' when not available.)
  --
  -- Use 'reifyExpress' when defining this.
  conjureExpress :: a -> Expr -> Expr

  conjureEvaluate :: (Expr->Expr) -> Int -> Defn -> Expr -> Maybe a
  conjureEvaluate  =  devaluate


-- | To be used in the implementation of 'conjureSubTypes'.
--
-- > instance ... => Conjurable <Type> where
-- >   ...
-- >   conjureSubTypes x  =  conjureType (field1 x)
-- >                      .  conjureType (field2 x)
-- >                      .  ...
-- >                      .  conjureType (fieldN x)
-- >   ...
conjureType :: Conjurable a => a -> Reification
conjureType x ms  =
  if hole x `elem` [h | (h,_,_,_,_,_) <- ms]
  then ms
  else conjureSubTypes x $ conjureReification1 x : ms

-- | like 'conjureType' but without type repetitions
nubConjureType :: Conjurable a => a -> Reification
nubConjureType x  =  nubOn (\(eh,_,_,_,_,_) -> eh) . conjureType x
-- The use of nubOn above is O(n^2).
-- So long as there is not a huge number of subtypes of a, so we're fine.

-- | Conjures a 'Reification1' for a 'Conjurable' type.
--
-- This is used in the implementation of 'conjureReification'.
conjureReification1 :: Conjurable a => a -> Reification1
conjureReification1 x  =  (hole x, conjureEquality x, conjureTiers x, names x, null $ conjureCases x, value "conjureSize" (conjureSize -:> x))

-- | Conjures a list of 'Reification1'
--   for a 'Conjurable' type, its subtypes and 'Bool'.
--
-- This is used in the implementation of
-- 'conjureHoles',
-- 'conjureMkEquation',
-- 'conjureAreEqual',
-- 'conjureTiersFor',
-- 'conjureIsDeconstructor',
-- 'conjureNamesFor',
-- 'conjureIsUnbreakable',
-- etc.
conjureReification :: Conjurable a => a -> [Reification1]
conjureReification x  =  nubConjureType x [conjureReification1 bool]
  where
  bool :: Bool
  bool  =  error "conjureReification: evaluated proxy boolean value (definitely a bug)"

-- | Reifies equality '==' in a 'Conjurable' type instance.
--
-- This is to be used
-- in the definition of 'conjureEquality'
-- of 'Conjurable' typeclass instances:
--
-- > instance ... => Conjurable <Type> where
-- >   ...
-- >   conjureEquality  =  reifyEquality
-- >   ...
reifyEquality :: (Eq a, Typeable a) => a -> Maybe Expr
reifyEquality  =  Just . head . reifyEq

-- | Reifies equality to be used in a conjurable type.
--
-- This is to be used
-- in the definition of 'conjureTiers'
-- of 'Conjurable' typeclass instances:
--
-- > instance ... => Conjurable <Type> where
-- >   ...
-- >   conjureTiers  =  reifyTiers
-- >   ...
reifyTiers :: (Listable a, Show a, Typeable a) => a -> Maybe [[Expr]]
reifyTiers  =  Just . mkExprTiers

-- | Reifies the 'expr' function in a 'Conjurable' type instance.
--
-- This is to be used
-- in the definition of 'conjureExpress'
-- of 'Conjurable' typeclass instances.
--
-- > instance ... => Conjurable <Type> where
-- >   ...
-- >   conjureExpress  =  reifyExpress
-- >   ...
reifyExpress :: (Express a, Show a) => a -> Expr -> Expr
reifyExpress a e  =  case value "expr" (expr -:> a) $$ e of
  Nothing -> e         -- TODO: consider throwing an error
  Just e' -> eval e e' -- TODO: consider throwing an error

mkExprTiers :: (Listable a, Show a, Typeable a) => a -> [[Expr]]
mkExprTiers a  =  mapT val (tiers -: [[a]])

-- | Computes a list of holes encoded as 'Expr's
--   from a 'Conjurable' functional value.
--
-- (cf. 'Conjure.Prim.cjHoles')
conjureHoles :: Conjurable f => f -> [Expr]
conjureHoles f  =  [eh | (eh,_,Just _,_,_,_) <- conjureReification f]

-- | Computes a function that makes an equation between two expressions.
conjureMkEquation :: Conjurable f => f -> Expr -> Expr -> Expr
conjureMkEquation f  =  mkEquation [eq | (_,Just eq,_,_,_,_) <- conjureReification f]

conjureDynamicEq :: Conjurable f => f -> Dynamic
conjureDynamicEq f  =  case conjureMkEquation f efxs efxs of
                       (Value "==" deq :$ _ :$ _) -> deq
                       _ -> error "conjureDynamicEq: expected an == but found something else.  Bug!"
  where
  efxs  =  conjureApplication "f" f

-- | Given a 'Conjurable' functional value,
--   computes a function that checks whether two 'Expr's are equal
--   up to a given number of tests.
conjureAreEqual :: Conjurable f => f -> Int -> Expr -> Expr -> Bool
conjureAreEqual f maxTests  =  (===)
  where
  (-==-)  =  conjureMkEquation f
  e1 === e2  =  isTrue $ e1 -==- e2
  isTrue  =  all (errorToFalse . eval False) . gs
  gs  =  take maxTests . grounds (conjureTiersFor f)

-- | Compute 'tiers' of values encoded as 'Expr's
--   of the type of the given 'Expr'.
conjureTiersFor :: Conjurable f => f -> Expr -> [[Expr]]
conjureTiersFor f e  =  tf allTiers
  where
  allTiers :: [ [[Expr]] ]
  allTiers  =  [etiers | (_,_,Just etiers,_,_,_) <- conjureReification f]
  tf []  =  [[e]] -- no tiers found, keep variable
  tf (etiers:etc)  =  case etiers of
                      ((e':_):_) | typ e' == typ e -> etiers
                      _                            -> tf etc

-- | Compute variable names for the given 'Expr' type.
conjureNamesFor :: Conjurable f => f -> Expr -> [String]
conjureNamesFor f e  =  head
                     $  [ns | (eh, _, _, ns, _, _) <- conjureReification f, typ e == typ eh]
                     ++ [names (undefined :: Int)] -- use [Int] on lists

conjureMostGeneralCanonicalVariation :: Conjurable f => f -> Expr -> Expr
conjureMostGeneralCanonicalVariation f  =  canonicalizeWith (conjureNamesFor f)
                                        .  fastMostGeneralVariation

-- | Checks if an expression is a deconstruction.
--
-- There should be a single 'hole' in the expression.
--
-- It should decrease the size of all arguments that have
-- a size greater than 0.
--
-- (cf. 'conjureIsDeconstructor')
conjureIsDeconstruction :: Conjurable f => f -> Int -> Expr -> Bool
conjureIsDeconstruction f maxTests ed  =  length (holes ed) == 1
                                       && typ h == typ ed
                                       && all is gs
  where
  gs  =  take maxTests $ grounds (conjureTiersFor f) ed
  [h]  =  holes ed
  sz  =  head [sz | (_, _, _, _, _, sz) <- conjureReification f
                  , isWellTyped (sz :$ h)]
  esz e  =  eval (0::Int) (sz :$ e)
  x << 0  =  True
  x << y  =  x < y
  is e  =  errorToFalse $ esz e << esz (holeValue e)
  holeValue e  =  fromMaybe err
               .  lookup h
               .  fromMaybe err
               $  e `match` ed
  err  =  error "conjureIsDeconstructor: the impossible happened"


-- | Compute candidate deconstructions from an 'Expr'.
--
-- This is used in the implementation of 'Conjure.Engine.candidateDefnsC'
-- followed by 'conjureIsDeconstruction'.
--
-- > > candidateDeconstructionsFrom (xx `mod'` yy)
-- > [ _ `mod` y
-- > , x `mod` _
-- > ]
--
-- To be constrasted with 'candidateDeconstructionsFromHoled'.
candidateDeconstructionsFrom :: Expr -> [Expr]
candidateDeconstructionsFrom e  =
  [ e'
  | v <- vars e
  , typ v == typ e
  , let e' = e //- [(v, holeAsTypeOf v)]
  , length (holes e') == 1
  ]

-- | Compute candidate deconstructions from an 'Expr'.
--
-- This is used in the implementation of 'Conjure.Engine.candidateExprs'
-- followed by 'conjureIsDeconstruction'.
--
-- This is similar to 'canonicalVariations'
-- but always leaves a hole
-- of the same return type as the given expression.
--
-- > > candidateDeconstructionsFrom (i_ `mod'` i_)
-- > [ _ `mod` x
-- > , x `mod` _
-- > ]
--
-- To be contrasted with 'candidateDeconstructionsFrom'
candidateDeconstructionsFromHoled :: Expr -> [Expr]
candidateDeconstructionsFromHoled e  =  map (//- [(v, h)])
                                     $  concatMap canonicalVariations
                                     $  deholings v e
  where
  h  =  holeAsTypeOf e
  v  =  "_#_" `varAsTypeOf` e  -- a marker variable with an invalid name
  -- at some point I should get rid of candidateDeconstructionsFrom in favour
  -- of this one

-- | Checks if an 'Expr' is of an unbreakable type.
conjureIsUnbreakable :: Conjurable f => f -> Expr -> Bool
conjureIsUnbreakable f e  =  head
  [is | (h,_,_,_,is,_) <- conjureReification f, typ h == typ e]

instance Conjurable () where
  conjureExpress   =  reifyExpress
  conjureEquality  =  reifyEquality
  conjureTiers     =  reifyTiers
  conjureCases _   =  [val ()]

instance Conjurable Bool where
  conjureExpress   =  reifyExpress
  conjureEquality  =  reifyEquality
  conjureTiers     =  reifyTiers
  conjureCases _   =  [val False, val True]

instance Conjurable Int where
  conjureExpress   =  reifyExpress
  conjureEquality  =  reifyEquality
  conjureTiers     =  reifyTiers
  conjureSize      =  size            where  size x | x < 0      =  0
                                                    | otherwise  =  x

-- allows easy modification of the global size function for integer values
-- duplicated above in the Int instance for performance reasons
integralSize :: Integral a => a -> Int
integralSize  =  fromIntegral . size  where  size x | x < 0      =  0
                                                    | otherwise  =  x

instance Conjurable Integer where
  conjureExpress   =  reifyExpress
  conjureEquality  =  reifyEquality
  conjureTiers     =  reifyTiers
  conjureSize      =  integralSize

instance Conjurable Char where
  conjureExpress   =  reifyExpress
  conjureEquality  =  reifyEquality
  conjureTiers     =  reifyTiers

-- bind equality to the given argument type
(==:) :: (a -> a -> Bool) -> a -> (a -> a -> Bool)
(==:)  =  const

-- the reconstruction of equality functions for polymorphic types
-- such as [a], (a,b), Maybe a, Either a b
-- is only needed so we don't impose an Eq restriction on the type context.

instance (Conjurable a, Listable a, Express a, Show a) => Conjurable [a] where
  conjureExpress   =  reifyExpress
  conjureSubTypes xs  =  conjureType (head xs)
  conjureTiers     =  reifyTiers
  conjureSize      =  length
  conjureCases xs  =  [ val ([] -: xs)
                      , value ":" ((:) ->>: xs) :$ hole x :$ hole xs
                      ]  where  x  =  head xs
  conjureEquality xs  =  from <$> conjureEquality x
    where
    x  =  head xs
    from e  =  value "==" (==)
      where
      (.==.)  =  evl e ==: x
      []     == []     = True
      (x:xs) == []     = False
      []     == (y:ys) = False
      (x:xs) == (y:ys) = x .==. y && xs == ys

instance ( Conjurable a, Listable a, Show a, Express a
         , Conjurable b, Listable b, Show b, Express b
         ) => Conjurable (a,b) where
  conjureExpress   =  reifyExpress
  conjureTiers     =  reifyTiers
  conjureSubTypes xy  =  conjureType (fst xy)
                      .  conjureType (snd xy)
  conjureCases xy  =  [value "," ((,) ->>: xy) :$ hole x :$ hole y]
    where
    (x,y) = (undefined,undefined) -: xy
  conjureEquality xy  =  from <$> conjureEquality x <*> conjureEquality y
    where
    (x,y)  =  xy
    from e1 e2  =  value "==" (==)
      where
      (==.)  =  evl e1 ==: x
      (.==)  =  evl e2 ==: y
      (x1,y1) == (x2,y2)  =  x1 ==. x2 && y1 .== y2


instance ( Conjurable a, Listable a, Show a, Express a
         , Conjurable b, Listable b, Show b, Express b
         , Conjurable c, Listable c, Show c, Express c
         ) => Conjurable (a,b,c) where
  conjureExpress   =  reifyExpress
  conjureTiers     =  reifyTiers
  conjureSubTypes xyz =  conjureType x
                      .  conjureType y
                      .  conjureType z
                      where (x,y,z) = xyz
  conjureCases xyz  =  [value ",," ((,,) ->>>: xyz) :$ hole x :$ hole y :$ hole z]
    where
    (x,y,z) = (undefined,undefined,undefined) -: xyz
  conjureEquality xyz  =  from
                      <$> conjureEquality x
                      <*> conjureEquality y
                      <*> conjureEquality z
    where
    (x,y,z)  =  xyz
    from e1 e2 e3  =  value "==" (==)
      where
      (==..)  =  evl e1 ==: x
      (.==.)  =  evl e2 ==: y
      (..==)  =  evl e3 ==: z
      (x1,y1,z1) == (x2,y2,z2)  =  x1 ==.. x2
                                && y1 .==. y2
                                && z1 ..== z2

instance (Conjurable a, Listable a, Show a, Express a) => Conjurable (Maybe a) where
  conjureExpress   =  reifyExpress
  conjureTiers     =  reifyTiers
  conjureSubTypes mx  =  conjureType (fromJust mx)
  conjureCases mx  =  [ value "Nothing" (Nothing -: mx)
                      , value "Just" (Just ->: mx) :$ hole x
                      ]
    where
    Just x  =  undefined -: mx
  conjureEquality mx  =  from <$> conjureEquality x
    where
    x  =  fromJust mx
    from e  =  value "==" (==)
      where
      (.==.)  =  evl e ==: x
      Nothing  == Nothing   =  True
      Nothing  == (Just _)  =  False
      (Just _) == Nothing   =  False
      (Just x) == (Just y)  =  x .==. y


instance ( Conjurable a, Listable a, Show a, Express a
         , Conjurable b, Listable b, Show b, Express b
         ) => Conjurable (Either a b) where
  conjureExpress   =  reifyExpress
  conjureTiers     =  reifyTiers
  conjureSubTypes elr  =  conjureType l . conjureType r
    where
    Left l   =  elr
    Right r  =  elr
  conjureCases exy  =  [ value "Left" (Left ->: exy) :$ hole x
                       , value "Right" (Right ->: exy) :$ hole y
                       ]
    where
    x  =  Left undefined -: exy
    y  =  Right undefined -: exy
  conjureEquality elr  =  from <$> conjureEquality l <*> conjureEquality r
    where
    Left l   =  elr
    Right r  =  elr
    from el er  =  value "==" (==)
      where
      (==.)  =  evl el ==: l
      (.==)  =  evl er ==: r
      (Left x)  == (Left y)   =  x ==. y
      (Left _)  == (Right _)  =  False
      (Right _) == (Left _)   =  False
      (Right x) == (Right y)  =  x .== y

instance (Conjurable a, Conjurable b) => Conjurable (a -> b) where
  conjureArgumentHoles f  =  hole (argTy f) : conjureArgumentHoles (f undefined)
  conjureSubTypes f  =  conjureType (argTy f) . conjureType (resTy f)
  conjureIf f  =  conjureIf (f undefined)
  conjureArgumentCases f  =  conjureCases (argTy f) : conjureArgumentCases (f undefined)
  conjureExpress f e
    | typ e == typeOf (argTy f)  =  conjureExpress (argTy f) e
    | otherwise                  =  conjureExpress (f undefined) e
  conjureEvaluate exprExpr mx defn ef  =  mf
    where
    ce  =  conjureEvaluate exprExpr mx defn
    mf  =  case ce (holeAsTypeOf ef :$ hole x) -: Just (f x) of
           Nothing -> Nothing
           Just _  -> Just $ \x -> fromMaybe err . ce $ ef :$ exprExpr (value "" x)
    f  =  undefined -: fromJust mf
    x  =  argTy f
    err  =  error "conjureEvaluate (a->b): BUG!  This should never be evaluated as it is protected by the outer case."

argTy :: (a -> b) -> a
argTy _  =  undefined

resTy :: (a -> b) -> b
resTy _  =  undefined

-- | Evaluates a 'Defn' into a regular Haskell value
--   returning 'Nothing' when there's a type mismatch.
--
-- The integer argument indicates the limit of recursive evaluations.
cevaluate :: Conjurable f => Int -> Defn -> Maybe f
cevaluate mx defn  =  mr
  where
  mr  =  conjureEvaluate exprExpr mx defn ef'
  exprExpr  =  conjureExpress $ fromJust mr
  (ef':_)  =  unfoldApp . fst $ head defn

-- | Evaluates a 'Defn' into a regular Haskell value
--   returning the given default value when there's a type mismatch.
--
-- The integer argument indicates the limit of recursive evaluations.
ceval :: Conjurable f => Int -> f -> Defn -> f
ceval mx z  =  fromMaybe z . cevaluate mx

-- | Evaluates a 'Defn' into a regular Haskell value
--   raising an error there's a type mismatch.
--
-- The integer argument indicates the limit of recursive evaluations.
cevl :: Conjurable f => Int -> Defn -> f
cevl mx  =  ceval mx err
  where
  err  =  error "cevl: type mismatch"

-- | Computes a complete application for the given function.
--
-- > > conjureApplication "not" not
-- > not p :: Bool
--
-- > > conjureApplication "+" ((+) :: Int -> Int -> Int)
-- > x + y :: Int
--
-- (cf. 'conjureVarApplication')
conjureApplication :: Conjurable f => String -> f -> Expr
conjureApplication  =  conjureWhatApplication value

-- | Computes a complete application for a variable
--   of the same type of the given function.
--
-- > > conjureVarApplication "not" not
-- > not p :: Bool
--
-- > > conjureVarApplication "+" ((+) :: Int -> Int -> Int)
-- > x + y :: Int
--
-- (cf. 'conjureApplication')
conjureVarApplication :: Conjurable f => String -> f -> Expr
conjureVarApplication  =  conjureWhatApplication var

-- | Used in the implementation of 'conjureApplication' and 'conjureVarApplication'.
conjureWhatApplication :: Conjurable f => (String -> f -> Expr) -> String -> f -> Expr
conjureWhatApplication what nm f  =  mostGeneralCanonicalVariation . foldApp
                                  $  what nf f : zipWith varAsTypeOf nas (conjureArgumentHoles f)
  where
  (nf:nas)  =  words nm ++ repeat ""


-- | Computes tiers of sets of patterns for the given function.
--
-- > > conjurePats [zero] "f" (undefined :: Int -> Int)
-- > [[[f x :: Int]],[[f 0 :: Int,f x :: Int]]]
conjurePats :: Conjurable f => [Expr] -> String -> f -> [[ [Expr] ]]
conjurePats es nm f  =  mapT (map mkApp . prods) $ cs
  where
  mkApp  =  foldApp . (ef:)
         .  unfold
         .  conjureMostGeneralCanonicalVariation f
         .  fold
  ef  =  var (head $ words nm) f  -- TODO: take the tail into account
  cs  =  products $ conjureArgumentPats es f


-- | Computes tiers of sets of patterns for the given function.
--
-- > > conjurePats [zero] "f" (undefined :: Int -> Int)
-- > [[[f x :: Int]],[[f 0 :: Int,f x :: Int]]]
newConjurePats :: Conjurable f => [Expr] -> String -> f -> [[ [Expr] ]]
newConjurePats es nm f  =  mapT (map mkApp)
                        $  combinePatternOptions
                        $  conjureArgumentPats es f
  where
  mkApp  =  foldApp . (ef:)
         .  unfold
         .  conjureMostGeneralCanonicalVariation f
         .  fold
  ef  =  var (head $ words nm) f  -- TODO: take the tail into account

  -- What a horrible enumeration hack below...
  -- Good luck to anyone who plans to refactor this.

  -- after application of mkApp, we end up w/  [[ [Pat   ] ]]
  combinePatternOptions :: [ [[ [Expr] ]] ] -> [[ [[Expr]] ]]
  combinePatternOptions []            =  [[[[]]]]
  combinePatternOptions (esss:essss)  =  concatPrefixesWithT esss
                                      $  combinePatternOptions essss

  -- The three functions below are all transformations over the same type
  -- [[ [[Expr]] ]]
  -- That's tiers of complete LHS of function definitions.
  -- We proceed right to left building all possible patterns.

  -- concatenates all of the possibilities of prefixing
  -- from tiers of possibilities
  concatPrefixesWithT :: [[[Expr]]] -> [[ [[Expr]] ]] -> [[ [[Expr]] ]]
  concatPrefixesWithT esss r  =  concatMapT (`concatPrefixesWith` r) esss

  -- concatenates the possibilities of prefixing from a list of prefixes
  concatPrefixesWith :: [Expr] -> [[ [[Expr]] ]] -> [[ [[Expr]] ]]
  concatPrefixesWith es r  =  mapT concat $ products [prefixWith e r | e <- es]

  -- prefixes with the given expression
  prefixWith :: Expr -> [[ [[Expr]] ]] -> [[ [[Expr]] ]]
  prefixWith e  =  mapT (map (e:))

  -- this was useful in figuring out the implementations
  -- of the local functions above
  --
  -- > prefixWith2 :: Expr -> Expr -> [[Bs]] -> [[Bs]]
  -- > prefixWith2 e1 e2 r  =  productWith (++) (mapT (map (e1:)) r)
  -- >                                          (mapT (map (e2:)) r)
  --
  -- A prefixWith3 would follow similarly.

  -- To debug the above function use:
  -- > import Test.LeanCheck.Tiers (printTiers)
  -- > printTiers 100 $ newConjurePats [zero] "f" (undefined :: Int -> Int -> Int)


-- | Returns a list of tiers of possible patterns for each argument.
--
-- The outer list has the same number of elements as the number of arguments
-- of the given function.
--
-- This function is internal and only used in the implementation of 'conjurePats'.
-- It may be removed from the API without further notice.
-- It has been temporarily promoted to public to help refactor 'conjurePats'.
conjureArgumentPats :: Conjurable f => [Expr] -> f -> [ [[ [Expr] ]] ]
conjureArgumentPats es f = zipWith mk (conjureArgumentHoles f) (conjureArgumentCases f)
  where
  -- deal with types that have no cases such as ints.
  mk h []  =  mapT (++ [h]) $ setsOf [[e] | e <- es, typ e == typ h]
  -- deal with types that have cases, such as lists, maybes, etc.
  mk h cs  =  [[[h]], [cs]]


prods :: [[a]] -> [[a]]
prods  =  foldr (productWith (:)) [[]]
  where
  productWith (?) xs ys  =  [x ? y | x <- xs, y <- ys]


-- -- -- other Conjurable instances -- -- --

instance Conjurable Ordering where
  conjureExpress   =  reifyExpress
  conjureEquality  =  reifyEquality
  conjureTiers     =  reifyTiers

instance Conjurable Float where
  conjureExpress   =  reifyExpress
  conjureEquality  =  reifyEquality
  conjureTiers     =  reifyTiers
  conjureSize      =  round

instance Conjurable Double where
  conjureExpress   =  reifyExpress
  conjureEquality  =  reifyEquality
  conjureTiers     =  reifyTiers
  conjureSize      =  round

instance Conjurable Int8 where
  conjureExpress   =  reifyExpress
  conjureEquality  =  reifyEquality
  conjureTiers     =  reifyTiers
  conjureSize      =  integralSize

instance Conjurable Int16 where
  conjureExpress   =  reifyExpress
  conjureEquality  =  reifyEquality
  conjureTiers     =  reifyTiers
  conjureSize      =  integralSize

instance Conjurable Int32 where
  conjureExpress   =  reifyExpress
  conjureEquality  =  reifyEquality
  conjureTiers     =  reifyTiers
  conjureSize      =  integralSize

instance Conjurable Int64 where
  conjureExpress   =  reifyExpress
  conjureEquality  =  reifyEquality
  conjureTiers     =  reifyTiers
  conjureSize      =  integralSize

instance Conjurable Word where
  conjureExpress   =  reifyExpress
  conjureEquality  =  reifyEquality
  conjureTiers     =  reifyTiers
  conjureSize      =  integralSize

instance Conjurable Word8 where
  conjureExpress   =  reifyExpress
  conjureEquality  =  reifyEquality
  conjureTiers     =  reifyTiers
  conjureSize      =  integralSize

instance Conjurable Word16 where
  conjureExpress   =  reifyExpress
  conjureEquality  =  reifyEquality
  conjureTiers     =  reifyTiers
  conjureSize      =  integralSize

instance Conjurable Word32 where
  conjureExpress   =  reifyExpress
  conjureEquality  =  reifyEquality
  conjureTiers     =  reifyTiers
  conjureSize      =  integralSize

instance Conjurable Word64 where
  conjureExpress   =  reifyExpress
  conjureEquality  =  reifyEquality
  conjureTiers     =  reifyTiers
  conjureSize      =  integralSize

instance (Integral a, Conjurable a, Listable a, Show a, Eq a, Express a) => Conjurable (Ratio a) where
  conjureExpress   =  reifyExpress
  conjureEquality  =  reifyEquality
  conjureTiers     =  reifyTiers
  conjureSize q    =  conjureSize (numerator q) + conjureSize (denominator q)
  conjureSubTypes q  =  conjureType (numerator q)
  conjureCases q  =  [value "%" ((%) ->>: q) :$ hole n :$ hole d]
    where
    n  =  numerator q
    d  =  denominator q

instance (RealFloat a, Conjurable a, Listable a, Show a, Eq a, Express a) => Conjurable (Complex a) where
  conjureExpress   =  reifyExpress
  conjureEquality  =  reifyEquality
  conjureTiers     =  reifyTiers
  conjureSize x    =  conjureSize (realPart x) + conjureSize (imagPart x)
  conjureSubTypes x  =  conjureType (realPart x)


-- Conjurable helper types --
instance Conjurable A where
  conjureExpress   =  reifyExpress
  conjureEquality  =  reifyEquality
  conjureTiers     =  reifyTiers
  conjureSize      =  integralSize

instance Conjurable B where
  conjureExpress   =  reifyExpress
  conjureEquality  =  reifyEquality
  conjureTiers     =  reifyTiers
  conjureSize      =  integralSize

instance Conjurable C where
  conjureExpress   =  reifyExpress
  conjureEquality  =  reifyEquality
  conjureTiers     =  reifyTiers
  conjureSize      =  integralSize

instance Conjurable D where
  conjureExpress   =  reifyExpress
  conjureEquality  =  reifyEquality
  conjureTiers     =  reifyTiers
  conjureSize      =  integralSize

instance Conjurable E where
  conjureExpress   =  reifyExpress
  conjureEquality  =  reifyEquality
  conjureTiers     =  reifyTiers
  conjureSize      =  integralSize

instance Conjurable F where
  conjureExpress   =  reifyExpress
  conjureEquality  =  reifyEquality
  conjureTiers     =  reifyTiers
  conjureSize      =  integralSize


-- Conjurable tuples --

instance ( Conjurable a, Listable a, Show a, Express a
         , Conjurable b, Listable b, Show b, Express b
         , Conjurable c, Listable c, Show c, Express c
         , Conjurable d, Listable d, Show d, Express d
         ) => Conjurable (a,b,c,d) where
  conjureExpress   =  reifyExpress
  conjureTiers     =  reifyTiers
  conjureSubTypes xyzw =  conjureType x
                       .  conjureType y
                       .  conjureType z
                       .  conjureType w
                       where (x,y,z,w) = xyzw
  conjureEquality xyzw  =  from
                       <$> conjureEquality x
                       <*> conjureEquality y
                       <*> conjureEquality z
                       <*> conjureEquality w
    where
    (x,y,z,w)  =  xyzw
    from e1 e2 e3 e4  =  value "==" (==)
      where
      (==...)  =  evl e1 ==: x
      (.==..)  =  evl e2 ==: y
      (..==.)  =  evl e3 ==: z
      (...==)  =  evl e4 ==: w
      (x1,y1,z1,w1) == (x2,y2,z2,w2)  =  x1 ==... x2
                                      && y1 .==.. y2
                                      && z1 ..==. z2
                                      && w1 ...== w2

instance ( Conjurable a, Listable a, Show a, Express a
         , Conjurable b, Listable b, Show b, Express b
         , Conjurable c, Listable c, Show c, Express c
         , Conjurable d, Listable d, Show d, Express d
         , Conjurable e, Listable e, Show e, Express e
         ) => Conjurable (a,b,c,d,e) where
  conjureExpress   =  reifyExpress
  conjureTiers     =  reifyTiers
  conjureSubTypes xyzwv =  conjureType x
                        .  conjureType y
                        .  conjureType z
                        .  conjureType w
                        .  conjureType v
                        where (x,y,z,w,v) = xyzwv
  conjureEquality xyzwv  =  from
                        <$> conjureEquality x
                        <*> conjureEquality y
                        <*> conjureEquality z
                        <*> conjureEquality w
                        <*> conjureEquality v
    where
    (x,y,z,w,v)  =  xyzwv
    from e1 e2 e3 e4 e5  =  value "==" (==)
      where
      (==....)  =  evl e1 ==: x
      (.==...)  =  evl e2 ==: y
      (..==..)  =  evl e3 ==: z
      (...==.)  =  evl e4 ==: w
      (....==)  =  evl e5 ==: v
      (x1,y1,z1,w1,v1) == (x2,y2,z2,w2,v2)  =  x1 ==.... x2
                                            && y1 .==... y2
                                            && z1 ..==.. z2
                                            && w1 ...==. w2
                                            && v1 ....== v2

instance ( Conjurable a, Listable a, Show a, Express a
         , Conjurable b, Listable b, Show b, Express b
         , Conjurable c, Listable c, Show c, Express c
         , Conjurable d, Listable d, Show d, Express d
         , Conjurable e, Listable e, Show e, Express e
         , Conjurable f, Listable f, Show f, Express f
         ) => Conjurable (a,b,c,d,e,f) where
  conjureExpress   =  reifyExpress
  conjureTiers     =  reifyTiers
  conjureSubTypes xyzwvu =  conjureType x
                         .  conjureType y
                         .  conjureType z
                         .  conjureType w
                         .  conjureType v
                         .  conjureType u
                         where (x,y,z,w,v,u) = xyzwvu
  conjureEquality xyzwvu  =  from
                         <$> conjureEquality x
                         <*> conjureEquality y
                         <*> conjureEquality z
                         <*> conjureEquality w
                         <*> conjureEquality v
                         <*> conjureEquality u
    where
    (x,y,z,w,v,u)  =  xyzwvu
    from e1 e2 e3 e4 e5 e6  =  value "==" (==)
      where
      (==.....)  =  evl e1 ==: x
      (.==....)  =  evl e2 ==: y
      (..==...)  =  evl e3 ==: z
      (...==..)  =  evl e4 ==: w
      (....==.)  =  evl e5 ==: v
      (.....==)  =  evl e6 ==: u
      (x1,y1,z1,w1,v1,u1) == (x2,y2,z2,w2,v2,u2)  =  x1 ==..... x2
                                                  && y1 .==.... y2
                                                  && z1 ..==... z2
                                                  && w1 ...==.. w2
                                                  && v1 ....==. v2
                                                  && u1 .....== u2

instance ( Conjurable a, Listable a, Show a, Express a
         , Conjurable b, Listable b, Show b, Express b
         , Conjurable c, Listable c, Show c, Express c
         , Conjurable d, Listable d, Show d, Express d
         , Conjurable e, Listable e, Show e, Express e
         , Conjurable f, Listable f, Show f, Express f
         , Conjurable g, Listable g, Show g, Express g
         ) => Conjurable (a,b,c,d,e,f,g) where
  conjureExpress   =  reifyExpress
  conjureTiers     =  reifyTiers
  conjureSubTypes xyzwvut =  conjureType x
                          .  conjureType y
                          .  conjureType z
                          .  conjureType w
                          .  conjureType v
                          .  conjureType u
                          .  conjureType t
                         where (x,y,z,w,v,u,t) = xyzwvut
  conjureEquality xyzwvut  =  from
                          <$> conjureEquality x
                          <*> conjureEquality y
                          <*> conjureEquality z
                          <*> conjureEquality w
                          <*> conjureEquality v
                          <*> conjureEquality u
                          <*> conjureEquality t
    where
    (x,y,z,w,v,u,t)  =  xyzwvut
    from e1 e2 e3 e4 e5 e6 e7  =  value "==" (==)
      where
      (==......)  =  evl e1 ==: x
      (.==.....)  =  evl e2 ==: y
      (..==....)  =  evl e3 ==: z
      (...==...)  =  evl e4 ==: w
      (....==..)  =  evl e5 ==: v
      (.....==.)  =  evl e6 ==: u
      (......==)  =  evl e7 ==: t
      (x1,y1,z1,w1,v1,u1,t1) == (x2,y2,z2,w2,v2,u2,t2)  =  x1 ==...... x2
                                                        && y1 .==..... y2
                                                        && z1 ..==.... z2
                                                        && w1 ...==... w2
                                                        && v1 ....==.. v2
                                                        && u1 .....==. u2
                                                        && t1 ......== t2

-- TODO: go up to 12-tuples

instance Name A
instance Name B
instance Name C
instance Name D
instance Name E
instance Name F
