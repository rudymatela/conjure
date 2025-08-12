-- tree.hs: conjuring functions over trees
--
-- Copyright (C) 2021-2025 Rudy Matela
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
{-# LANGUAGE CPP, TemplateHaskell #-}

import Conjure
import Test.LeanCheck
import Data.Express hiding (height,size)

data Tree  =  Leaf
           |  Node Tree Int Tree
  deriving (Eq, Ord, Show, Read)

nil :: Tree -> Bool
nil Leaf  =  True
nil _     =  False

size :: Tree -> Int
size Leaf  =  0
size (Node Leaf _ Leaf)  =  1
size (Node (Node Leaf _ Leaf) _ Leaf)  =  2
size (Node Leaf _ (Node Leaf _ Leaf))  =  2
size (Node (Node Leaf _ Leaf) _ (Node Leaf _ Leaf))  =  3

height :: Tree -> Int
height (Node Leaf _ Leaf)  =  0
height (Node (Node Leaf _ Leaf) _ Leaf)  =  1
height (Node Leaf _ (Node Leaf _ Leaf))  =  1
height (Node (Node Leaf _ Leaf) _ (Node Leaf _ Leaf))  =  1
height (Node (Node Leaf _ (Node Leaf _ Leaf)) _ (Node Leaf _ Leaf))  =  2

leftmost :: Tree -> Int
leftmost (Node l x _)  =  if nil l then x else leftmost l

rightmost :: Tree -> Int
rightmost (Node _ x r)  =  if nil r then x else rightmost r

ordered :: Tree -> Bool
ordered Leaf  =  True
ordered (Node l x r)  =  (nil l || rightmost l < x)
                      && (nil r || x < leftmost r)
                      && ordered l
                      && ordered r

preorder :: Tree -> [Int]
preorder Leaf =  []
preorder (Node l x r)  =  [x] ++ preorder l ++ preorder r

inorder :: Tree -> [Int]
inorder Leaf  =  []
inorder (Node l x r)  =  inorder l ++ [x] ++ inorder r

posorder :: Tree -> [Int]
posorder Leaf =  []
posorder (Node l x r)  =  posorder l ++ posorder r ++ [x]


-- this mem searches both sides of the tree
mem :: Int -> Tree -> Bool
mem _ Leaf  =  False
mem y (Node l x r)  =  y == x || mem y l || mem y r


-- the following instances could have been derived with:
-- deriveConjurable ''Tree

deriveExpress ''Tree

instance Listable Tree where
  tiers  =  cons0 Leaf
        \/  cons3 Node

instance Name Tree where
  name _  =  "t1"

instance Conjurable Tree where
  conjureExpress   =  reifyExpress
  conjureEquality  =  reifyEquality
  conjureTiers     =  reifyTiers
  conjureSubTypes x  =  conjureType (undefined :: Int)
  conjureSize     =  size
  conjureCases t  =  [ val (Leaf -: t)
                     , value "Node" (Node ->>>: t) :$ hole l :$ hole x :$ hole r
                     ]
    where
    Node l x r  =  Node undefined undefined undefined -: t


main :: IO ()
main = do
  conjure "size" size ingredients
  conjure "height" height ingredients
  conjure "preorder" preorder ingredients
  conjure "inorder" inorder ingredients
  conjure "posorder" posorder ingredients
  conjure "leftmost" leftmost ingredients
  conjure "rightmost" rightmost ingredients

  conjure "mem" mem
    [ con False
    , fun "||" (||)
    , fun "==" ((==) :: Int -> Int -> Bool)
    ]

  -- unreachable: needs size 22 but OOMs at 19/20 (v0.5.16)
  conjure "ordered" ordered
    [ con True
    , con False
    , fun "&&" (&&)
    , fun "||" (||)
    , fun "<" ((<) :: Int -> Int -> Bool)
    , fun "rightmost" rightmost
    , fun "leftmost" leftmost
    , fun "nil" nil
    , maxSize 12
    ]

  conjure "ordered" ordered
    [ fun "strictlyOrdered" (strictlyOrdered :: [Int] -> Bool)
    , fun "inorder" inorder
    ]


ingredients :: [Ingredient]
ingredients  =
  [ con (0 :: Int)
  , con (1 :: Int)
  , con (-1 :: Int)
  , fun "+" ((+) :: Int -> Int -> Int)
  , fun "max" (max :: Int -> Int -> Int)

  , fun "nil" nil
  , guard

  , con ([] :: [Int])
  , fun ":" ((:) :: Int -> [Int] -> [Int])
  , fun "++" ((++) :: [Int] -> [Int] -> [Int])

  , maxTests 1080
  ]


strictlyOrdered :: [Int] -> Bool
strictlyOrdered []  =  True
strictlyOrdered (x:xs)  =  (null xs || x < head xs) && strictlyOrdered xs
