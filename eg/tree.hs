-- tree.hs: conjuring functions over trees
--
-- Copyright (C) 2021 Rudy Matela
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
{-# LANGUAGE CPP, TemplateHaskell #-}
#if __GLASGOW_HASKELL__ == 708
{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving #-}
import Data.Typeable (Typeable)
#endif

import Conjure
import Test.LeanCheck
import Data.Express hiding (height,size)

-- TODO: remove the following import
-- and fix build on GHC 7.10 and 7.8
-- the generation of -: and ->>>: somehow fails.
import Test.LeanCheck.Utils

data Tree  =  Leaf
           |  Node Tree Int Tree
  deriving (Eq, Ord, Show, Read)

#if __GLASGOW_HASKELL__ == 708
deriving instance Typeable Tree
#endif

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

-- member of binary search tree
member :: Int -> Tree -> Bool
member _ Leaf  =  False
member y (Node l x r)  =  y == x || (if y < x then member y l else member y r)

insert :: Int -> Tree -> Tree
insert x Leaf  =  unit x
insert x (Node l y r)  =  case compare x y of
  LT -> Node (insert x l) y r
  EQ -> Node l y r
  GT -> Node l y (insert x r)


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
  conjureCases t  =  [ val (Leaf -: t)
                     , value "Node" (Node ->>>: t) :$ hole l :$ hole x :$ hole r
                     ]
    where
    Node l x r  =  Node undefined undefined undefined -: t


main :: IO ()
main = do
  conjure "leftmost" leftmost
    [ prim "undefined" (undefined :: Int)
    , prif (undefined :: Int)
    , prim "nil" nil
    ]

  conjure "rightmost" rightmost
    [ prim "undefined" (undefined :: Int)
    , prif (undefined :: Int)
    , prim "nil" nil
    ]

  conjure "size" size
    [ pr (0 :: Int)
    , pr (1 :: Int)
    , prim "+" ((+) :: Int -> Int -> Int)
    , prim "nil" nil
    , prim "left" left
    , prim "right" right
    ]

  conjure "height" height
    [ pr (0 :: Int)
    , pr (1 :: Int)
    , pr (-1 :: Int)
    , prim "+" ((+) :: Int -> Int -> Int)
    , prim "max" (max :: Int -> Int -> Int)
    , prim "nil" nil
    , prim "left" left
    , prim "right" right
    ]

  conjure "mem" mem
    [ pr False
    , prim "||" (||)
    , prim "==" ((==) :: Int -> Int -> Bool)
    ]

  conjureWithMaxSize 15 "member" member
    [ pr False
    , prim "||" (||)
    , prim "==" ((==) :: Int -> Int -> Bool)
    , prim "<" ((<) :: Int -> Int -> Bool)
    , prif (undefined :: Bool)
    ]

  -- simply out of reach performance-wise (reaching 16 but need size 26)
  conjureWithMaxSize 12 "insert" insert
    [ pr Leaf
    , prim "Node" Node
    , prim "unit" unit
    , prim "==" ((==) :: Int -> Int -> Bool)
    , prim "<" ((<) :: Int -> Int -> Bool)
    , prif (undefined :: Tree)
    ]

  -- out of reach performance-wise (reaching 16 but need 19)
  conjureWithMaxSize 12 "before" before
    [ pr Leaf
    , prim "Node" Node
    , prim "==" ((==) :: Int -> Int -> Bool)
    , prim "<" ((<) :: Int -> Int -> Bool)
    , prif (undefined :: Tree)
    ]

  -- reaching before through some "cheating"
  conjureWithMaxSize 16 "before" before
    [ pr Leaf
    , prim "Node" Node
    , prim "`compare`" (compare :: Int -> Int -> Ordering)
    , prim "case" (caseOrdering :: Ordering -> Tree -> Tree -> Tree -> Tree)
    ]

  -- reaching beyond through some "cheating"
  conjureWithMaxSize 16 "beyond" beyond
    [ pr Leaf
    , prim "Node" Node
    , prim "`compare`" (compare :: Int -> Int -> Ordering)
    , prim "case" (caseOrdering :: Ordering -> Tree -> Tree -> Tree -> Tree)
    ]

  -- out of reach (reaching 7 but need 13)
  conjureWithMaxSize 6 "union" union
    [ pr Leaf
    , prim "Node" Node
    , prim "before" before
    , prim "beyond" beyond
    ]
  -- maybe with invariant following test data there will be more pruning
  -- properties?

caseOrdering :: Ordering -> a -> a -> a -> a
caseOrdering o lt eq gt  =  case o of
                            LT -> lt
                            EQ -> eq
                            GT -> gt

before :: Int -> Tree -> Tree
before _ Leaf  =  Leaf
before y (Node l x r)  =  case y `compare` x of
                          LT -> before y l
                          EQ -> l
                          GT -> Node l x (before y r)
-- single-line view:
-- before y (Node l x r)  =  case y `compare` x of LT -> before y l; EQ -> l; GT -> Node l x (before y r)

beyond :: Int -> Tree -> Tree
beyond _ Leaf  =  Leaf
beyond y (Node l x r)  =  case x `compare` y of
                          LT -> beyond y r
                          EQ -> r
                          GT -> Node (beyond y l) x r

union :: Tree -> Tree -> Tree
union t Leaf  =  t
union t (Node l x r)  =  Node (union (before x t) l) x (union (beyond x t) r)

-- same as insert, but using an if instead of a case:
insertIf :: Int -> Tree -> Tree
insertIf x Leaf  =  unit x                                   -- 2
insertIf x (Node l y r)  =  if x == y                        -- 6
                            then Node l y r                  -- 10
                            else if x < y                    -- 14
                                 then Node (insert x l) y r  -- 20
                                 else Node l y (insert x r)  -- 26
