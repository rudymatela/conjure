-- |
-- Module      : Conjure.Arguable
-- Copyright   : (c) 2021 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- This module is part of 'Arguable'.
--
-- This defines the 'Arguable' typeclass
-- and utilities involving it.
--
-- You are probably better off importing "Conjure".
module Conjure.Arguable
  ( Arguable (..)
  , instances

  , mkEq1
  , mkEq2
  , mkEq3
  , mkEq4
  , mkOrd1
  , mkOrd2
  , mkOrd3
  , mkOrd4

  , (*==*)
  , (*/=*)
  , (*<=*)
  , (*<*)

  , Listable (..)
  , module Conjure.Expr
  )
where

import Data.Maybe
import Data.Ratio

import Test.LeanCheck
import Test.LeanCheck.Utils

import Conjure.Expr hiding (application)
import Conjure.TypeBinding
import Test.Speculate.Expr


-- |
-- Extrapolate can generalize counter-examples of any types that are
-- 'Arguable'.
--
-- 'Arguable' types must first be instances of
--
-- * 'Listable', so Extrapolate knows how to enumerate values;
-- * 'Express', so Extrapolate can represent then manipulate values;
-- * 'Name', so Extrapolate knows how to name variables.
--
-- There are no required functions, so we can define instances with:
--
-- > instance Arguable OurType
--
-- However, it is desirable to define both 'background' and 'subInstances'.
--
-- The following example shows a datatype and its instance:
--
-- > data Stack a = Stack a (Stack a) | Empty
--
-- > instance Arguable a => Arguable (Stack a) where
-- >   subInstances s  =  instances (argTy1of1 s)
--
-- To declare 'instances' it may be useful to use type binding
-- operators such as: 'argTy1of1', 'argTy1of2' and 'argTy2of2'.
--
-- Instances can be automatically derived using
-- 'Conjure.Arguable.Derive.deriveArguable'
-- which will also automatically derivate
-- 'Listable', 'Express' and 'Name' when needed.
class (Listable a, Express a, Name a, Show a) => Arguable a where
  -- | List of symbols allowed to appear in side-conditions.
  --   Defaults to @[]@.  See 'value'.
  background :: a -> [Expr]
  background _  =  []

  -- | Computes a list of reified subtype instances.
  --   Defaults to 'id'.
  --   See 'instances'.
  subInstances :: a -> Instances -> Instances
  subInstances _  =  id


instance Arguable ()

instance Arguable Bool where
  background p  =  reifyEq p
                ++ [ value "not" not 
                   , value "&&" (&&)
                   , value "||" (||)
                   ]

instance Arguable Int where
  background x  =  reifyEqOrd x
                ++ [ value "+" ((+) -:> x)
                   , value "*" ((*) -:> x)
                   , value "-" ((-) -:> x)
                   , value "if" (\b x y -> if b then x else y :: Int)
                   ]

instance Arguable Integer where
  background x  =  reifyEqOrd x

instance Arguable Char where
  background c  =  reifyEqOrd c

instance Arguable a => Arguable (Maybe a) where
  background mx  =  mkEq1  (maybeEq  ->:> mx)
                 ++ mkOrd1 (maybeOrd ->:> mx)
                 ++ [ value "Just" (Just ->: mx) ]
  subInstances mx  =  instances (fromJust mx)

-- TODO: move maybeEq and maybeOrd here, I'll have to change the tests

instance (Arguable a, Arguable b) => Arguable (Either a b) where
  background exy  =  mkEq2  (eitherEq  ->>:> exy)
                  ++ mkOrd2 (eitherOrd ->>:> exy)
                  ++ [ value "Left"  (Left  ->: exy)
                     , value "Right" (Right ->: exy) ]
  subInstances exy  =  instances (fromLeft  exy)
                    .  instances (fromRight exy)

-- TODO: move eitherEq and eitherOrd here, I'll have to change the tests

instance (Arguable a, Arguable b) => Arguable (a,b) where
  background xy  =  mkEq2  (pairEq  ->>:> xy)
                 ++ mkOrd2 (pairOrd ->>:> xy)
  subInstances xy  =  instances (fst xy)
                   .  instances (snd xy)

instance (Arguable a, Arguable b, Arguable c)
      => Arguable (a,b,c) where
  background xyz  =  mkEq3  (tripleEq  ->>>:> xyz)
                  ++ mkOrd3 (tripleOrd ->>>:> xyz)
  subInstances xyz  =  instances x . instances y . instances z
                       where (x,y,z) = xyz

instance (Arguable a, Arguable b, Arguable c, Arguable d)
      => Arguable (a,b,c,d) where
  background xyzw  =  mkEq4  (quadrupleEq  ->>>>:> xyzw)
                   ++ mkOrd4 (quadrupleOrd ->>>>:> xyzw)
  subInstances xyzw  =  instances x
                     .  instances y
                     .  instances z
                     .  instances w
                     where (x,y,z,w) = xyzw

instance Arguable a => Arguable [a] where
  background xs  =  mkEq1  (listEq  ->:> xs)
                 ++ mkOrd1 (listOrd ->:> xs)
                 ++ [ value "length" (length -:> xs) ]
                 ++ [ value "elem"      (elemBy (*==*) ->:> xs) | hasEq $ head xs ]
  subInstances xs  =  instances (head xs)

instance Arguable Ordering where
  background o  =  reifyEqOrd o

mkEq1 :: (Arguable a, Arguable b)
      => ((b -> b -> Bool) -> a -> a -> Bool) -> [Expr]
mkEq1 m = takeWhile (\_ -> hasEq x) . mkEq $ m (*==*)
  where
  x = arg1 ==: m

mkEq2 :: (Arguable a, Arguable b, Arguable c)
      => ((b -> b -> Bool) -> (c -> c -> Bool) -> a -> a -> Bool) -> [Expr]
mkEq2 m = takeWhile (\_ -> hasEq x && hasEq y) . mkEq $ m (*==*) (*==*)
  where
  x = arg1 ==: m
  y = arg2 ==: m

mkEq3 :: (Arguable a, Arguable b, Arguable c, Arguable d)
      => ((b->b->Bool) -> (c->c->Bool) -> (d->d->Bool) -> a -> a -> Bool)
      -> [Expr]
mkEq3 m = takeWhile (\_ -> hasEq x && hasEq y && hasEq z) . mkEq
        $ m (*==*) (*==*) (*==*)
  where
  x = arg1 ==: m
  y = arg2 ==: m
  z = arg3 ==: m

mkEq4 :: (Arguable a, Arguable b, Arguable c, Arguable d, Arguable e)
      => ((b->b->Bool) -> (c->c->Bool) -> (d->d->Bool) -> (e->e->Bool) -> a -> a -> Bool)
      -> [Expr]
mkEq4 m = takeWhile (\_ -> hasEq x && hasEq y && hasEq z && hasEq w) . mkEq
        $ m (*==*) (*==*) (*==*) (*==*)
  where
  x = arg1 ==: m
  y = arg2 ==: m
  z = arg3 ==: m
  w = arg4 ==: m

mkOrd1 :: (Arguable a, Arguable b)
       => ((b -> b -> Bool) -> a -> a -> Bool) -> [Expr]
mkOrd1 m = takeWhile (\_ -> hasOrd x) . mkOrdLessEqual
         $ m (*<=*)
  where
  x = arg1 ==: m

mkOrd2 :: (Arguable a, Arguable b, Arguable c)
       => ((b -> b -> Bool) -> (c -> c -> Bool) -> a -> a -> Bool) -> [Expr]
mkOrd2 m = takeWhile (\_ -> hasOrd x && hasOrd y) . mkOrdLessEqual
         $ m (*<=*) (*<=*)
  where
  x = arg1 ==: m
  y = arg2 ==: m

mkOrd3 :: (Arguable a, Arguable b, Arguable c, Arguable d)
       => ((b->b->Bool) -> (c->c->Bool) -> (d->d->Bool) -> a -> a -> Bool)
       -> [Expr]
mkOrd3 m = takeWhile (\_ -> hasOrd x && hasOrd y && hasOrd z) . mkOrdLessEqual
         $ m (*<=*) (*<=*) (*<=*)
  where
  x = arg1 ==: m
  y = arg2 ==: m
  z = arg3 ==: m

mkOrd4 :: (Arguable a, Arguable b, Arguable c, Arguable d, Arguable e)
       => ((b->b->Bool) -> (c->c->Bool) -> (d->d->Bool) -> (e->e->Bool) -> a -> a -> Bool)
       -> [Expr]
mkOrd4 m = takeWhile (\_ -> hasOrd x && hasOrd y && hasOrd z && hasOrd w) . mkOrdLessEqual
         $ m (*<=*) (*<=*) (*<=*) (*<=*)
  where
  x = arg1 ==: m
  y = arg2 ==: m
  z = arg3 ==: m
  w = arg4 ==: m

-- | Usage: @ins "x" (undefined :: Type)@
ins :: Arguable a => a -> Instances
ins x = reifyListable x
     ++ reifyName x
     ++ reifyBackground x

-- | Used in the definition of 'subInstances'
--   in 'Arguable' typeclass instances.
instances :: Arguable a => a -> Instances -> Instances
instances x = this x (subInstances x)
  where
  this :: Arguable a
       => a -> (Instances -> Instances) -> Instances -> Instances
  this x f is =
    if isListableT is (typeOf x)
      then is
      else f (ins x ++ is)
  -- TODO: change type to a -> [Instances -> Instances] -> Instances -> Instances

reifyBackground :: Arguable a => a -> Instances
reifyBackground x = [value "background" (background x)]

fromBackgroundOf :: (Arguable a, Typeable b) => String -> a -> Maybe b
fromBackgroundOf nm = listToMaybe
                    . mapMaybe evaluate
                    . filter (`isConstantNamed` nm)
                    . background

hasEq :: Arguable a => a -> Bool
hasEq x = isJust $ "==" `fromBackgroundOf` x -: mayb (x >- x >- bool)

hasOrd :: Arguable a => a -> Bool
hasOrd x = isJust $ "<=" `fromBackgroundOf` x -: mayb (x >- x >- bool)

(*==*) :: Arguable a => a -> a -> Bool
x *==* y = x == y
  where
  (==) = fromMaybe (error "(*==*): no (==) operator in background")
       $ "==" `fromBackgroundOf` x

(*/=*) :: Arguable a => a -> a -> Bool
x */=* y = x /= y
  where
  (/=) = fromMaybe (error "(*/=*): no (/=) operator in background")
       $ "/=" `fromBackgroundOf` x

(*<=*) :: Arguable a => a -> a -> Bool
x *<=* y = x <= y
  where
  (<=) = fromMaybe (error "(*<=*): no (<=) operator in background")
       $ "<=" `fromBackgroundOf` x

(*<*) :: Arguable a => a -> a -> Bool
x *<* y = x < y
  where
  (<) = fromMaybe (error "(*<*): no (<) operator in background")
       $ "<" `fromBackgroundOf` x

-- -- other Arguable instances -- --

instance (Integral a, Arguable a) => Arguable (Ratio a)

instance ( Arguable a, Arguable b, Arguable c, Arguable d
         , Arguable e )
      => Arguable (a,b,c,d,e) where
  subInstances xyzwv = instances x . instances y . instances z
                     . instances w . instances v
                     where (x,y,z,w,v) = xyzwv

instance ( Arguable a, Arguable b, Arguable c, Arguable d
         , Arguable e, Arguable f )
      => Arguable (a,b,c,d,e,f) where
  subInstances xyzwvu = instances x . instances y . instances z
                      . instances w . instances v . instances u
               where (x,y,z,w,v,u) = xyzwvu

instance ( Arguable a, Arguable b, Arguable c, Arguable d
         , Arguable e, Arguable f, Arguable g )
      => Arguable (a,b,c,d,e,f,g) where
  subInstances xyzwvut = instances x . instances y . instances z . instances w
                       . instances v . instances u . instances t
                where (x,y,z,w,v,u,t) = xyzwvut

#if __GLASGOW_HASKELL__ < 710
-- No 8-tuples for you:
-- On GHC 7.8, 8-tuples are not Typeable instances.  We could add a standalone
-- deriving clause, but that may cause trouble if some other library does the
-- same.  User should declare Generalizable 8-tuples manually when using GHC <=
-- 7.8.
#else
instance ( Arguable a, Arguable b, Arguable c, Arguable d
         , Arguable e, Arguable f, Arguable g, Arguable h )
      => Arguable (a,b,c,d,e,f,g,h) where
  subInstances xyzwvuts = instances x . instances y . instances z . instances w
                        . instances v . instances u . instances t . instances s
    where (x,y,z,w,v,u,t,s) = xyzwvuts
#endif
