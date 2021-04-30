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
  , canonicalApplication
  , canonicalVarApplication
  , conjureHoles
  , conjureIfs
  , conjureTiersFor
  , conjureMkEquation
  )
where

import Test.LeanCheck
import Test.LeanCheck.Utils
import Conjure.Expr hiding (application)
import Test.Speculate.Expr
import Data.Functor ((<$>))
import Control.Applicative ((<*>))

import Data.Int     -- for instances
import Data.Word    -- for instances
import Data.Ratio   -- for instance
import Data.Complex -- for instance


-- | Single reification of some functions over a type as 'Expr's.
--
-- A hole, an if function, an equality function and tiers.
type Reification1  =  (Expr, Expr, Maybe Expr, Maybe [[Expr]])

-- | A reification over a collection of types.
--
-- Represented as a transformation of a list to a list.
type Reification  =  [Reification1] -> [Reification1]


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


conjureType :: Conjurable a => a -> Reification
conjureType x ms  =
  if hole x `elem` [h | (h,_,_,_) <- ms]
  then ms
  else conjureSubTypes x $ conjureReification1 x : ms

conjureReification1 :: Conjurable a => a -> Reification1
conjureReification1 x  =  (hole x, ifFor x, conjureEquality x, conjureTiers x)

conjureReification :: Conjurable a => a -> [Reification1]
conjureReification x  =  conjureType x [conjureReification1 bool]
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

mkExprTiers :: (Listable a, Show a, Typeable a) => a -> [[Expr]]
mkExprTiers a  =  mapT val (tiers -: [[a]])

conjureHoles :: Conjurable f => f -> [Expr]
conjureHoles f  =  nub [eh | (eh,_,_,Just _) <- conjureReification f]

conjureIfs :: Conjurable f => f -> [Expr]
conjureIfs f  =  [eef | (_,eef,_,Just _) <- conjureReification f]

conjureMkEquation :: Conjurable f => f -> Expr -> Expr -> Expr
conjureMkEquation f  =  mkEquation [eq | (_,_,Just eq,_) <- conjureReification f]

conjureTiersFor :: Conjurable f => f -> Expr -> [[Expr]]
conjureTiersFor f e  =  tf allTiers
  where
  allTiers :: [ [[Expr]] ]
  allTiers  =  [etiers | (_,_,_,Just etiers) <- conjureReification f]
  tf []  =  [[e]] -- no tiers found, keep variable
  tf (etiers:etc)  =  case etiers of
                      ((e':_):_) | typ e' == typ e -> etiers
                      _                            -> tf etc


instance Conjurable () where
  conjureEquality  =  reifyEquality
  conjureTiers     =  reifyTiers

instance Conjurable Bool where
  conjureEquality  =  reifyEquality
  conjureTiers     =  reifyTiers

instance Conjurable Int where
  conjureEquality  =  reifyEquality
  conjureTiers     =  reifyTiers

instance Conjurable Integer where
  conjureEquality  =  reifyEquality
  conjureTiers     =  reifyTiers

instance Conjurable Char where
  conjureEquality  =  reifyEquality
  conjureTiers     =  reifyTiers

-- bind equality to the given argument type
(==:) :: (a -> a -> Bool) -> a -> (a -> a -> Bool)
(==:)  =  const

instance (Conjurable a, Listable a, Show a) => Conjurable [a] where
  conjureSubTypes xs  =  conjureType (head xs)
  conjureTiers     =  reifyTiers
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

instance ( Conjurable a, Listable a, Show a
         , Conjurable b, Listable b, Show b
         ) => Conjurable (a,b) where
  conjureTiers     =  reifyTiers
  conjureSubTypes xy  =  conjureType (fst xy)
                      .  conjureType (snd xy)
  conjureEquality xy  =  from <$> conjureEquality x <*> conjureEquality y
    where
    (x,y)  =  xy
    from e1 e2  =  value "==" (==)
      where
      (==.)  =  evl e1 ==: x
      (.==)  =  evl e2 ==: y
      (x1,y1) == (x2,y2)  =  x1 ==. x2 && y1 .== y2


instance ( Conjurable a, Listable a, Show a
         , Conjurable b, Listable b, Show b
         , Conjurable c, Listable c, Show c
         ) => Conjurable (a,b,c) where
  conjureTiers     =  reifyTiers
  conjureSubTypes xyz =  conjureType x
                      .  conjureType y
                      .  conjureType z
                      where (x,y,z) = xyz
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

instance (Conjurable a, Listable a, Show a) => Conjurable (Maybe a) where
  conjureTiers     =  reifyTiers
  conjureSubTypes mx  =  conjureType (fromJust mx)
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


instance ( Conjurable a, Listable a, Show a
         , Conjurable b, Listable b, Show b
         ) => Conjurable (Either a b) where
  conjureTiers     =  reifyTiers
  conjureSubTypes elr  =  conjureType l . conjureType r
    where
    Left l   =  elr
    Right r  =  elr
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

instance (Listable a, Show a, Conjurable a, Conjurable b) => Conjurable (a -> b) where
  conjureArgumentHoles f  =  hole (argTy f) : conjureArgumentHoles (f undefined)
  conjureSubTypes f  =   conjureType (argTy f) . conjureType (resTy f)

argTy :: (a -> b) -> a
argTy _  =  undefined

resTy :: (a -> b) -> b
resTy _  =  undefined

canonicalArgumentVariables :: Conjurable f => f -> [Expr]
canonicalArgumentVariables  =  unfoldApp
                            .  mostGeneralCanonicalVariation
                            .  foldApp
                            .  conjureArgumentHoles

canonicalApplication :: Conjurable f => String -> f -> Expr
canonicalApplication nm f  =  foldApp (value nm f : canonicalArgumentVariables f)

canonicalVarApplication :: Conjurable f => String -> f -> Expr
canonicalVarApplication nm f  =  foldApp (var nm f : canonicalArgumentVariables f)



-- -- -- other Conjurable instances -- -- --

instance Conjurable Ordering where
  conjureEquality  =  reifyEquality
  conjureTiers     =  reifyTiers

instance Conjurable Float where
  conjureEquality  =  reifyEquality
  conjureTiers     =  reifyTiers

instance Conjurable Double where
  conjureEquality  =  reifyEquality
  conjureTiers     =  reifyTiers

instance Conjurable Int8 where
  conjureEquality  =  reifyEquality
  conjureTiers     =  reifyTiers

instance Conjurable Int16 where
  conjureEquality  =  reifyEquality
  conjureTiers     =  reifyTiers

instance Conjurable Int32 where
  conjureEquality  =  reifyEquality
  conjureTiers     =  reifyTiers

instance Conjurable Int64 where
  conjureEquality  =  reifyEquality
  conjureTiers     =  reifyTiers

instance Conjurable Word where
  conjureEquality  =  reifyEquality
  conjureTiers     =  reifyTiers

instance Conjurable Word8 where
  conjureEquality  =  reifyEquality
  conjureTiers     =  reifyTiers

instance Conjurable Word16 where
  conjureEquality  =  reifyEquality
  conjureTiers     =  reifyTiers

instance Conjurable Word32 where
  conjureEquality  =  reifyEquality
  conjureTiers     =  reifyTiers

instance Conjurable Word64 where
  conjureEquality  =  reifyEquality
  conjureTiers     =  reifyTiers

instance (Integral a, Conjurable a, Listable a, Show a, Eq a) => Conjurable (Ratio a) where
  conjureEquality  =  reifyEquality
  conjureTiers     =  reifyTiers
  conjureSubTypes q  =  conjureType (numerator q)

instance (RealFloat a, Conjurable a, Listable a, Show a, Eq a) => Conjurable (Complex a) where
  conjureEquality  =  reifyEquality
  conjureTiers     =  reifyTiers
  conjureSubTypes x  =  conjureType (realPart x)


-- Conjurable tuples --

instance ( Conjurable a, Listable a, Show a
         , Conjurable b, Listable b, Show b
         , Conjurable c, Listable c, Show c
         , Conjurable d, Listable d, Show d
         ) => Conjurable (a,b,c,d) where
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

instance ( Conjurable a, Listable a, Show a
         , Conjurable b, Listable b, Show b
         , Conjurable c, Listable c, Show c
         , Conjurable d, Listable d, Show d
         , Conjurable e, Listable e, Show e
         ) => Conjurable (a,b,c,d,e) where
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

instance ( Conjurable a, Listable a, Show a
         , Conjurable b, Listable b, Show b
         , Conjurable c, Listable c, Show c
         , Conjurable d, Listable d, Show d
         , Conjurable e, Listable e, Show e
         , Conjurable f, Listable f, Show f
         ) => Conjurable (a,b,c,d,e,f) where
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

instance ( Conjurable a, Listable a, Show a
         , Conjurable b, Listable b, Show b
         , Conjurable c, Listable c, Show c
         , Conjurable d, Listable d, Show d
         , Conjurable e, Listable e, Show e
         , Conjurable f, Listable f, Show f
         , Conjurable g, Listable g, Show g
         ) => Conjurable (a,b,c,d,e,f,g) where
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
