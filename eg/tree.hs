-- tree.hs: conjuring functions over trees
--
-- Copyright (C) 2021 Rudy Matela
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ == 708
{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving #-}
import Data.Typeable (Typeable)
#endif

import Conjure
import Test.LeanCheck

data Tree  =  Leaf
           |  Node Tree Int Tree
  deriving (Eq, Ord, Show, Read)

#if __GLASGOW_HASKELL__ == 708
deriving instance Typeable Tree
#endif

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
leftmost (Node l x _)  =  if nil l then x else leftmost (left l)

rightmost :: Tree -> Int
rightmost (Node _ x r)  =  if nil r then x else rightmost (right r)

height :: Tree -> Int
height Leaf  =  -1
height (Node l _ r)  =  1 + max (height l) (height r)

size :: Tree -> Int
size Leaf  =  0
size (Node l _ r)  =  size l + 1 + size r

-- this mem searches both sides of the tree
mem :: Int -> Tree -> Bool
mem _ Leaf  =  False
mem y (Node l x r)  =  y == x || mem y l || mem y r

insert :: Int -> Tree -> Tree
insert x Leaf  =  unit x
insert x (Node l y r)  =  case compare x y of
  LT -> Node (insert x l) y r
  EQ -> Node l y r
  GT -> Node l y (insert x r)

-- TODO: mem alternative for binary search trees


instance Listable Tree where
  tiers  =  cons0 Leaf
        \/  cons3 Node

instance Conjurable Tree where
  conjureEquality  =  reifyEquality
  conjureTiers     =  reifyTiers
  conjureSubTypes x  =  conjureType (undefined :: Int)


main :: IO ()
main = do
  conjure "leftmost" leftmost
    [ value "valu" valu
    , value "nil" nil
    , value "left" left
    , value "right" right
    ]

  conjure "rightmost" rightmost
    [ value "valu" valu
    , value "nil" nil
    , value "left" left
    , value "right" right
    ]

  conjureWith args{maxRecursiveCalls=2, maxSize=13} "size" size
    [ val (0 :: Int)
    , val (1 :: Int)
    , value "+" ((+) :: Int -> Int -> Int)
    , value "nil" nil
    , value "left" left
    , value "right" right
    ]

  conjureWith args{maxRecursiveCalls=2, maxSize=13} "height" height
    [ val (0 :: Int)
    , val (1 :: Int)
    , val (-1 :: Int)
    , value "+" ((+) :: Int -> Int -> Int)
    , value "max" (max :: Int -> Int -> Int)
    , value "nil" nil
    , value "left" left
    , value "right" right
    ]

  -- out of reach performance-wise
  conjureWith args{maxRecursiveCalls=2, maxSize=12} "mem" mem
    [ val False
    , value "||" (||)
    , value "==" ((==) :: Int -> Int -> Bool)
    , value "nil" nil
    , value "left" left
    , value "right" right
    , value "valu" valu
    ]

  -- simply out of reach performance-wise (size 34)
  conjureWith args{maxRecursiveCalls=2, maxSize=9} "insert" mem
    [ val Leaf
    , value "Node" Node
    , value "left" left
    , value "right" right
    , value "valu" valu
    , value "nil" nil
    , value "unit" unit
    , value "==" ((==) :: Int -> Int -> Bool)
    , value "<" ((<) :: Int -> Int -> Bool)
    , value ">" ((>) :: Int -> Int -> Bool)
    ]


sizeIf :: Tree -> Int
sizeIf t  =  if nil t  -- 3
             then 0    -- 4
             else sizeIf (left t) + sizeIf (right t)
             --      5     6   7  8   9      10  11

heightIf :: Tree -> Int
heightIf t  =  if nil t  -- 3
               then -1   -- 4
               else 1 + max (height (left t)) (height (right t))
               --   5 6  7     8      9  10      11     12   13

memIf :: Int -> Tree -> Bool
memIf y t  =  if nil t     -- 3
              then False   -- 4
              else y == valu t || memIf y (left t) || memIf y (right t)
              --   5  6   7  8  9  10  11   12  13 14  15  16   17   18

insertIf :: Int -> Tree -> Tree
insertIf x t  =  if nil t                 -- 3
                 then unit x              -- 5
                 else if x == valu t      -- 10
                      then t              -- 11
                      else if x < valu t  -- 16
                           then Node (insert x (left t)) (valu t) (right t)  -- 25
                           else Node (left t) (valu t) (insert x (right t))  -- 34
