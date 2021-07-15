-- |
-- Module      : Conjure.Conjurable
-- Copyright   : (c) 2021 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- This module is part of 'Conjure'.
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
  , conjureIsDeconstructor
  , conjureIsUnbreakable
  , conjureReification
  , conjureReification1
  )
where

import Test.LeanCheck
import Test.LeanCheck.Utils
import Test.LeanCheck.Error (errorToFalse)
import Conjure.Expr hiding (application)
import Conjure.Cases
import Test.Speculate.Expr
import Data.Functor ((<$>))
import Control.Applicative ((<*>))

import Data.Int     -- for instances
import Data.Word    -- for instances
import Data.Ratio   -- for instance
import Data.Complex -- for instance


-- | Single reification of some functions over a type as 'Expr's.
--
-- A hole, an equality function and tiers.
type Reification1  =  (Expr, Maybe Expr, Maybe [[Expr]], Bool)

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
class Typeable a => Conjurable a where
  conjureArgumentHoles :: a -> [Expr]
  conjureArgumentHoles _  =  []

  -- | Returns 'Just' the '==' function encoded as an 'Expr' when available
  --   or 'Nothing' otherwise.
  conjureEquality :: a -> Maybe Expr
  conjureEquality _  =  Nothing

  -- | Returns 'Just' 'tiers' of values encoded as 'Expr's when possible
  --   or 'Nothing' otherwise.
  conjureTiers :: a -> Maybe [[Expr]]
  conjureTiers _  =  Nothing

  conjureSubTypes :: a -> Reification
  conjureSubTypes _  =  id

  conjureIf :: a -> Expr
  conjureIf   =  ifFor

  conjureCases :: a -> [Expr]
  conjureCases _  =  []

  conjureArgumentCases :: a -> [[Expr]]
  conjureArgumentCases _  =  []

  conjureExpress :: a -> Expr -> Expr


conjureType :: Conjurable a => a -> Reification
conjureType x ms  =
  if hole x `elem` [h | (h,_,_,_) <- ms]
  then ms
  else conjureSubTypes x $ conjureReification1 x : ms

-- | like 'conjureType' but without type repetitions
nubConjureType :: Conjurable a => a -> Reification
nubConjureType x  =  nubOn (\(eh,_,_,_) -> eh) . conjureType x
-- The use of nubOn above is O(n^2).
-- So long as there is not a huge number of subtypes of a, so we're fine.

conjureReification1 :: Conjurable a => a -> Reification1
conjureReification1 x  =  (hole x, conjureEquality x, conjureTiers x, null $ conjureCases x)

conjureReification :: Conjurable a => a -> [Reification1]
conjureReification x  =  nubConjureType x [conjureReification1 bool]
  where
  bool :: Bool
  bool  =  error "conjureReification: evaluated proxy boolean value (definitely a bug)"

-- | Reifies equality to be used in a conjurable type.
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

reifyExpress :: (Express a, Show a) => a -> Expr -> Expr
reifyExpress a e  =  case value "expr" (expr -:> a) $$ e of
  Nothing -> e         -- TODO: consider throwing an error
  Just e' -> eval e e' -- TODO: consider throwing an error

mkExprTiers :: (Listable a, Show a, Typeable a) => a -> [[Expr]]
mkExprTiers a  =  mapT val (tiers -: [[a]])

conjureHoles :: Conjurable f => f -> [Expr]
conjureHoles f  =  [eh | (eh,_,Just _,_) <- conjureReification f]

conjureMkEquation :: Conjurable f => f -> Expr -> Expr -> Expr
conjureMkEquation f  =  mkEquation [eq | (_,Just eq,_,_) <- conjureReification f]

conjureAreEqual :: Conjurable f => f -> Int -> Expr -> Expr -> Bool
conjureAreEqual f maxTests  =  (===)
  where
  (-==-)  =  conjureMkEquation f
  e1 === e2  =  isTrue $ e1 -==- e2
  isTrue  =  all (errorToFalse . eval False) . gs
  gs  =  take maxTests . grounds (conjureTiersFor f)

conjureTiersFor :: Conjurable f => f -> Expr -> [[Expr]]
conjureTiersFor f e  =  tf allTiers
  where
  allTiers :: [ [[Expr]] ]
  allTiers  =  [etiers | (_,_,Just etiers,_) <- conjureReification f]
  tf []  =  [[e]] -- no tiers found, keep variable
  tf (etiers:etc)  =  case etiers of
                      ((e':_):_) | typ e' == typ e -> etiers
                      _                            -> tf etc

conjureIsDeconstructor :: Conjurable f => f -> Int -> Expr -> Expr -> Expr -> Bool
conjureIsDeconstructor f maxTests  =  isDeconstructionE
                                   .  take maxTests
                                   .  grounds (conjureTiersFor f)

conjureIsUnbreakable :: Conjurable f => f -> Expr -> Bool
conjureIsUnbreakable f e  =  head
  [is | (h,_,_,is) <- conjureReification f, typ h == typ e]

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

instance Conjurable Integer where
  conjureExpress   =  reifyExpress
  conjureEquality  =  reifyEquality
  conjureTiers     =  reifyTiers

instance Conjurable Char where
  conjureExpress   =  reifyExpress
  conjureEquality  =  reifyEquality
  conjureTiers     =  reifyTiers

-- bind equality to the given argument type
(==:) :: (a -> a -> Bool) -> a -> (a -> a -> Bool)
(==:)  =  const

instance (Conjurable a, Listable a, Express a, Show a) => Conjurable [a] where
  conjureExpress   =  reifyExpress
  conjureSubTypes xs  =  conjureType (head xs)
  conjureTiers     =  reifyTiers
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
    x  =  Just undefined -: mx
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

argTy :: (a -> b) -> a
argTy _  =  undefined

resTy :: (a -> b) -> b
resTy _  =  undefined

conjureApplication :: Conjurable f => String -> f -> Expr
conjureApplication  =  conjureWhatApplication value

conjureVarApplication :: Conjurable f => String -> f -> Expr
conjureVarApplication  =  conjureWhatApplication var

conjureWhatApplication :: Conjurable f => (String -> f -> Expr) -> String -> f -> Expr
conjureWhatApplication what nm f  =  mostGeneralCanonicalVariation . foldApp
                                  $  what nf f : zipWith varAsTypeOf nas (conjureArgumentHoles f)
  where
  (nf:nas)  =  words nm ++ repeat ""

conjurePats :: Conjurable f => String -> f -> [[[Expr]]]
conjurePats nm f  =  mapT (map (foldApp . (ef:) . unfold . mostGeneralCanonicalVariation . fold) . prods) $ cs
  where
  ef  =  var nm f
  cs  =  products $ zipWith mk (conjureArgumentHoles f) (conjureArgumentCases f)
  mk h []  =  mapT (++ [h]) $ setsOf (tiersFor h)
  mk h cs  =  [[[h]], [cs]]
  tiersFor  =  conjureTiersFor f

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

instance Conjurable Double where
  conjureExpress   =  reifyExpress
  conjureEquality  =  reifyEquality
  conjureTiers     =  reifyTiers

instance Conjurable Int8 where
  conjureExpress   =  reifyExpress
  conjureEquality  =  reifyEquality
  conjureTiers     =  reifyTiers

instance Conjurable Int16 where
  conjureExpress   =  reifyExpress
  conjureEquality  =  reifyEquality
  conjureTiers     =  reifyTiers

instance Conjurable Int32 where
  conjureExpress   =  reifyExpress
  conjureEquality  =  reifyEquality
  conjureTiers     =  reifyTiers

instance Conjurable Int64 where
  conjureExpress   =  reifyExpress
  conjureEquality  =  reifyEquality
  conjureTiers     =  reifyTiers

instance Conjurable Word where
  conjureExpress   =  reifyExpress
  conjureEquality  =  reifyEquality
  conjureTiers     =  reifyTiers

instance Conjurable Word8 where
  conjureExpress   =  reifyExpress
  conjureEquality  =  reifyEquality
  conjureTiers     =  reifyTiers

instance Conjurable Word16 where
  conjureExpress   =  reifyExpress
  conjureEquality  =  reifyEquality
  conjureTiers     =  reifyTiers

instance Conjurable Word32 where
  conjureExpress   =  reifyExpress
  conjureEquality  =  reifyEquality
  conjureTiers     =  reifyTiers

instance Conjurable Word64 where
  conjureExpress   =  reifyExpress
  conjureEquality  =  reifyEquality
  conjureTiers     =  reifyTiers

instance (Integral a, Conjurable a, Listable a, Show a, Eq a, Express a) => Conjurable (Ratio a) where
  conjureExpress   =  reifyExpress
  conjureEquality  =  reifyEquality
  conjureTiers     =  reifyTiers
  conjureSubTypes q  =  conjureType (numerator q)

-- TODO: move Express (Complex a) to Data.Express
instance (Show a, Typeable a) => Express (Complex a) where  expr  =  val

instance (RealFloat a, Conjurable a, Listable a, Show a, Eq a, Express a) => Conjurable (Complex a) where
  conjureExpress   =  reifyExpress
  conjureEquality  =  reifyEquality
  conjureTiers     =  reifyTiers
  conjureSubTypes x  =  conjureType (realPart x)


-- Conjurable helper types --
instance Conjurable A where
  conjureExpress   =  reifyExpress
  conjureEquality  =  reifyEquality
  conjureTiers     =  reifyTiers

instance Conjurable B where
  conjureExpress   =  reifyExpress
  conjureEquality  =  reifyEquality
  conjureTiers     =  reifyTiers

instance Conjurable C where
  conjureExpress   =  reifyExpress
  conjureEquality  =  reifyEquality
  conjureTiers     =  reifyTiers

instance Conjurable D where
  conjureExpress   =  reifyExpress
  conjureEquality  =  reifyEquality
  conjureTiers     =  reifyTiers

instance Conjurable E where
  conjureExpress   =  reifyExpress
  conjureEquality  =  reifyEquality
  conjureTiers     =  reifyTiers

instance Conjurable F where
  conjureExpress   =  reifyExpress
  conjureEquality  =  reifyEquality
  conjureTiers     =  reifyTiers


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
