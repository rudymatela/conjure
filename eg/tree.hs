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

deriveExpress ''Tree

unit :: Int -> Tree
unit x  =  Node Leaf x Leaf

nil :: Tree -> Bool
nil Leaf  =  True
nil _     =  False

left :: Tree -> Tree
left (Node l _ _)  =  l

right :: Tree -> Tree
right (Node _ _ r)  =  r

valu :: Tree -> Int
valu (Node _ x _)  =  x


leftmost :: Tree -> Int
leftmost (Node l x _)  =  if nil l then x else leftmost l

rightmost :: Tree -> Int
rightmost (Node _ x r)  =  if nil r then x else rightmost r

height :: Tree -> Int
height Leaf  =  -1
height (Node l _ r)  =  1 + max (height l) (height r)

size :: Tree -> Int
size Leaf  =  0
size (Node l _ r)  =  size l + 1 + size r

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


instance Listable Tree where
  tiers  =  cons0 Leaf
        \/  cons3 Node

instance Name Tree where
  name _  =  "t1"

-- the following instance could have been derived with:
-- deriveConjurable ''Tree

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
  conjure "leftmost" leftmost
    [ fun "undefined" (undefined :: Int)
    , iif (undefined :: Int)
    , fun "nil" nil
    ]

  conjure "rightmost" rightmost
    [ fun "undefined" (undefined :: Int)
    , iif (undefined :: Int)
    , fun "nil" nil
    ]

  conjure "size" size
    [ unfun (0 :: Int)
    , unfun (1 :: Int)
    , fun "+" ((+) :: Int -> Int -> Int)
    , fun "nil" nil
    ]

  conjure "height" height
    [ unfun (0 :: Int)
    , unfun (1 :: Int)
    , unfun (-1 :: Int)
    , fun "+" ((+) :: Int -> Int -> Int)
    , fun "max" (max :: Int -> Int -> Int)
    , fun "nil" nil
    ]

  conjure "mem" mem
    [ unfun False
    , fun "||" (||)
    , fun "==" ((==) :: Int -> Int -> Bool)
    ]

  -- unreachable: needs size 22 but OOMs at 19/20 (v0.5.16)
  conjure "ordered" ordered
    [ unfun True
    , unfun False
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

  conjure "preorder" preorder
    [ unfun ([] :: [Int])
    , fun ":" ((:) :: Int -> [Int] -> [Int])
    , fun "++" ((++) :: [Int] -> [Int] -> [Int])
    ]

  conjure "inorder" inorder
    [ unfun ([] :: [Int])
    , fun ":" ((:) :: Int -> [Int] -> [Int])
    , fun "++" ((++) :: [Int] -> [Int] -> [Int])
    ]

  conjure "posorder" posorder
    [ unfun ([] :: [Int])
    , fun ":" ((:) :: Int -> [Int] -> [Int])
    , fun "++" ((++) :: [Int] -> [Int] -> [Int])
    ]


strictlyOrdered :: [Int] -> Bool
strictlyOrdered []  =  True
strictlyOrdered (x:xs)  =  (null xs || x < head xs) && strictlyOrdered xs
