-- Copyright (c) 2019-2025 Rudy Matela.
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- uncomment to debug derivation:
-- {-# OPTIONS_GHC -ddump-splices #-}

import Test hiding ((-:), (->:))
-- -: and ->: should be generated by deriveConjurable

data Choice  =  Ae | Bee | Cee deriving (Show, Eq, Typeable)
data Peano  =  Zero | Succ Peano deriving (Show, Eq, Typeable)
data Lst a  =  a :- Lst a | Nil deriving (Show, Eq, Typeable)
data Bush a  =  Bush a :-: Bush a | Leaf a deriving (Show, Eq, Typeable)
data Tree a  =  Node (Tree a) a (Tree a) | Null deriving (Show, Eq, Typeable)

deriveConjurable ''Choice
deriveConjurable ''Peano
deriveConjurable ''Lst
deriveConjurable ''Bush
deriveConjurable ''Tree

-- Nested datatype cascade
data Nested  =  Nested N0 (N1 Int) (N2 Int Int) deriving (Eq, Show, Typeable)
data N0      =  R0 Int deriving (Eq, Show, Typeable)
data N1 a    =  R1 a   deriving (Eq, Show, Typeable)
data N2 a b  =  R2 a b deriving (Eq, Show, Typeable)

deriveConjurableCascading ''Nested

-- Recursive nested datatype cascade
data RN       =  RN RN0 (RN1 Int) (RN2 Int RN) deriving (Eq, Show, Typeable)
data RN0      =  Nest0 Int | Recurse0 RN deriving (Eq, Show, Typeable)
data RN1 a    =  Nest1 a   | Recurse1 RN deriving (Eq, Show, Typeable)
data RN2 a b  =  Nest2 a b | Recurse2 RN deriving (Eq, Show, Typeable)
-- beware: values of the above type are always infinite!
--         derivation works but full evaluation does not terminate

deriveConjurableCascading ''RN

-- Those should have no effect (instance already exists):
{- uncommenting those should generate warnings
deriveConjurable ''Bool
deriveConjurable ''Maybe
deriveConjurable ''Either
-}

-- Those should not generate warnings
deriveConjurableIfNeeded ''Bool
deriveConjurableIfNeeded ''Maybe
deriveConjurableIfNeeded ''Either

data Mutual    =  Mutual0   | Mutual CoMutual deriving (Eq, Show, Typeable)
data CoMutual  =  CoMutual0 | CoMutual Mutual deriving (Eq, Show, Typeable)

deriveConjurableCascading ''Mutual


main :: IO ()
main  =  mainTest tests 5040

tests :: Int -> [Bool]
tests n  =
  [ True

  , conjurableOK (undefined :: Bool)
  , conjurableOK (undefined :: Int)
  , conjurableOK (undefined :: Char)
  , conjurableOK (undefined :: [Bool])
  , conjurableOK (undefined :: [Int])
  , conjurableOK (undefined :: String)

  , conjurableOK (undefined :: Choice)
  , conjurableOK (undefined :: Peano)
  , conjurableOK (undefined :: Lst Int)
  , conjurableOK (undefined :: Bush Int)
  , conjurableOK (undefined :: Tree Int)
--, conjurableOK (undefined :: RN) -- TODO: FIX: infinite loop somewhere...

  , conjureSize Ae == 1
  , conjureSize Bee == 1
  , conjureSize Cee == 1
  , conjureSize Zero == 1
  , conjureSize (Succ Zero) == 2
  , conjureSize (Succ (Succ Zero)) == 3
  , conjureSize (Nil :: Lst Int) == 1
  , conjureSize (10 :- (20 :- Nil) :: Lst Int) == 33

  , conjureCases choice
    == [ val Ae
       , val Bee
       , val Cee
       ]

  , conjureCases peano
    == [ val Zero
       , value "Succ" Succ :$ hole (undefined :: Peano)
       ]

  , conjureCases (lst int)
    == [ value ":-" ((:-) ->>: lst int) :$ hole int :$ hole (lst int)
       , val (Nil :: Lst Int)
       ]

  , conjureCases (bush int)
    == [ value ":-:" ((:-:) ->>: bush int) :$ hole (bush int) :$ hole (bush int)
       , value "Leaf" (Leaf ->: bush int) :$ hole int
       ]

  , conjureCases (tree int)
    == [ value "Node" (Node ->>>: tree int) :$ hole (tree int) :$ hole int :$ hole (tree int)
       , val (Null :: Tree Int)
       ]

  , conjureCases nested
    == [ value "Nested" Nested :$ hole n0 :$ hole (n1 int) :$ hole (n2 int int)
       ]

  , conjureHoles (undefined :: Choice) == [ hole (undefined :: Choice)
                                          , hole (undefined :: Bool)
                                          ]
  , conjureHoles (undefined :: Peano) == [ hole (undefined :: Peano)
                                         , hole (undefined :: Bool)
                                         ]
  , conjureHoles (undefined :: Lst Int) == [ hole (undefined :: Int)
                                            , hole (undefined :: Lst Int)
                                            , hole (undefined :: Bool)
                                            ]
  , conjureHoles (undefined :: Nested) == [ hole (undefined :: N0)
                                          , hole (undefined :: N1 Int)
                                          , hole (undefined :: Int)
                                          , hole (undefined :: N2 Int Int)
                                          , hole (undefined :: Nested)
                                          , hole (undefined :: Bool)
                                          ]
  , conjureHoles (undefined :: RN) == [ hole (undefined :: RN0)
                                      , hole (undefined :: RN1 Int)
                                      , hole (undefined :: Int)
                                      , hole (undefined :: RN2 Int RN)
                                      , hole (undefined :: RN)
                                      , hole (undefined :: Bool)
                                      ]
  , conjureHoles (undefined :: Mutual) == [ hole (undefined :: CoMutual)
                                          , hole (undefined :: Mutual)
                                          , hole (undefined :: Bool)
                                          ]
  , conjureHoles (undefined :: CoMutual) == [ hole (undefined :: Mutual)
                                            , hole (undefined :: CoMutual)
                                            , hole (undefined :: Bool)
                                            ]
  ]


-- proxies --
choice :: Choice
choice  =  undefined

peano :: Peano
peano  =  undefined

lst :: a -> Lst a
lst _  =  undefined

bush :: a -> Bush a
bush _  =  undefined

tree :: a -> Tree a
tree _  =  undefined

nested :: Nested
nested  =  undefined

n0 :: N0
n0  =  undefined

n1 :: a -> N1 a
n1 _  =  undefined

n2 :: a -> b -> N2 a b
n2 _ _  =  undefined
