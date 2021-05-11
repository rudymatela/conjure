-- |
-- Module      : Conjure.Spec
-- Copyright   : (c) 2021 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- An internal module of 'Conjure',
-- a library for Conjuring function implementations
-- from tests or partial definitions.
-- (a.k.a.: functional inductive programming)
module Conjure.Spec
  ( conjure1
  , conjure2
  , conjure3
  , conjure1With
  , conjure2With
  , conjure3With
  , (-=)
  , Spec1
  , Spec2
  , Spec3
  )
where

import Conjure.Engine
import Conjure.Conjurable
import Conjure.Utils


-- | A partial specification of a function with one argument:
--
-- > sumSpec :: Spec1 [Int] Int
-- > sumSpec  =
-- >   [ []      -= 0
-- >   , [1,2]   -= 3
-- >   , [3,4,5] -= 12
-- >   ]
--
-- To be passed as one of the arguments to 'conjure1'.
type Spec1 a b     = [(a,b)]

-- | A partial specification of a function with two arguments:
--
-- > appSpec :: Spec2 [Int] [Int] [Int]
-- > appSpec  =
-- >   [ (,) []      [0,1]   -= [0,1]
-- >   , (,) [2,3]   []      -= [2,3]
-- >   , (,) [4,5,6] [7,8,9] -= [4,5,6,7,8,9]
-- >   ]
--
-- To be passed as one of the arguments to 'conjure2'.
type Spec2 a b c   = [((a,b),c)]

-- | A partial specification of a function with three arguments.
--
-- To be passed as one of the arguments to 'conjure3'
type Spec3 a b c d = [((a,b,c),d)]

(-=) :: a -> b -> (a,b)
(-=)  =  (,)


-- | Conjures a function from a specification.
--
-- Given:
--
-- > sumSpec :: Spec1 [Int] Int
-- > sumSpec  =
-- >   [ []      -= 0
-- >   , [1,2]   -= 3
-- >   , [3,4,5] -= 12
-- >   ]
--
-- > sumPrimitives :: [Expr]
-- > sumPrimitives  =
-- >   [ value "null" (null :: [Int] -> Bool)
-- >   , val (0::Int)
-- >   , value "+"    ((+) :: Int -> Int -> Int)
-- >   , value "head" (head :: [Int] -> Int)
-- >   , value "tail" (tail :: [Int] -> [Int])
-- >   ]
--
-- Then:
--
-- > > conjure1 "sum" sumSpec sumPrimitives
-- > sum :: [Int] -> Int
-- > -- testing 3 combinations of argument values
-- > -- ...
-- > -- looking through 189/465 candidates of size 10
-- > xs ++ ys  =  if null xs then ys else head xs:(tail xs ++ ys)
--
-- (cf. 'Spec1', 'conjure1With')
conjure1 :: (Eq a, Show a, Conjurable a, Conjurable b)
         => String -> Spec1 a b -> [Expr] -> IO ()
conjure1  =  conjure1With args


conjure2 :: ( Conjurable a, Eq a, Show a
            , Conjurable b, Eq b, Show b
            , Conjurable c
            ) => String -> Spec2 a b c -> [Expr] -> IO ()
conjure2  =  conjure2With args


conjure3 :: ( Conjurable a, Eq a, Show a
            , Conjurable b, Eq b, Show b
            , Conjurable c, Eq c, Show c
            , Conjurable d
            ) => String -> Spec3 a b c d -> [Expr] -> IO ()
conjure3  =  conjure3With args


conjure1With :: (Eq a, Show a, Conjurable a, Conjurable b)
             => Args -> String -> Spec1 a b -> [Expr] -> IO ()
conjure1With args nm bs  =  conjureWith args{forceTests=ts} nm (mkFun1 bs)
  where
  ts = [[val x] | (x,_) <- bs]


conjure2With :: ( Conjurable a, Eq a, Show a
                , Conjurable b, Eq b, Show b
                , Conjurable c
                ) => Args -> String -> Spec2 a b c -> [Expr] -> IO ()
conjure2With args nm bs  =  conjureWith args{forceTests=ts} nm (mkFun2 bs)
  where
  ts = [[val x, val y] | ((x,y),_) <- bs]


conjure3With :: ( Conjurable a, Eq a, Show a
                , Conjurable b, Eq b, Show b
                , Conjurable c, Eq c, Show c
                , Conjurable d
                ) => Args -> String -> Spec3 a b c d -> [Expr] -> IO ()
conjure3With args nm bs  =  conjureWith args{forceTests=ts} nm (mkFun3 bs)
  where
  ts = [[val x, val y, val z] | ((x,y,z),_) <- bs]


mkFun1 :: Eq a => [(a,b)] -> a -> b
mkFun1 bs  =  \x -> fromMaybe undefined $ lookup x bs


mkFun2 :: (Eq a, Eq b) => [((a,b),c)] -> a -> b -> c
mkFun2 bs  =  \x y -> fromMaybe undefined $ lookup (x,y) bs


mkFun3 :: (Eq a, Eq b, Eq c) => [((a,b,c),d)] -> a -> b -> c -> d
mkFun3 bs  =  \x y z -> fromMaybe undefined $ lookup (x,y,z) bs
