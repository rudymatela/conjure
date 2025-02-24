-- bst.hs: conjuring functions over binary search trees (BSTs)
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


-- the following assume a binary search tree
mem :: Int -> Tree -> Bool
mem _ Leaf  =  False
mem y (Node l x r)  =  y == x || (if y < x then mem y l else mem y r)

insert :: Int -> Tree -> Tree
insert x Leaf          =  unit x  -- 2
insert x (Node l y r)  =
  case compare x y of             -- 6
  LT -> Node (insert x l) y r     -- 12
  EQ -> Node l y r                -- 16
  GT -> Node l y (insert x r)     -- 22

before :: Int -> Tree -> Tree
before _ Leaf  =  Leaf
before y (Node l x r)  =  case y `compare` x of
                          LT -> before y l
                          EQ -> l
                          GT -> Node l x (before y r)

beyond :: Int -> Tree -> Tree
beyond _ Leaf  =  Leaf
beyond y (Node l x r)  =  case x `compare` y of
                          LT -> beyond y r
                          EQ -> r
                          GT -> Node (beyond y l) x r

union :: Tree -> Tree -> Tree
union t Leaf  =  t
union t (Node l x r)  =  Node (union (before x t) l) x (union (beyond x t) r)


instance Listable Tree where
  tiers  =  cons0 Leaf
        \/  cons3 Node `suchThat` ordered

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
  conjure "mem" mem
    [ pr False
    , prim "||" (||)
    , prim "==" ((==) :: Int -> Int -> Bool)
    , prim "<" ((<) :: Int -> Int -> Bool)
    , guard
    ]

  conjure "mem" mem
    [ pr False
    , pr True
    , prim "`compare`" (compare :: Int -> Int -> Ordering)
    , primOrdCaseFor (undefined :: Bool)
    ]

  -- simply out of reach performance-wise (reaching 16 but need size 22)
  conjureWithMaxSize 12 "insert" insert
    [ pr Leaf
    , prim "Node" Node
    , prim "unit" unit
    , prim "`compare`" (compare :: Int -> Int -> Ordering)
    , primOrdCaseFor (undefined :: Tree)
    ]

  -- reachable in 15s, candidate #32878 at size 14.
  -- increase target to 50400 to reach...
  conjureFromSpecWith args{target=5040} "before" beforeSpec
    [ pr Leaf
    , prim "Node" Node
    , prim "==" ((==) :: Int -> Int -> Bool)
    , prim "<" ((<) :: Int -> Int -> Bool)
    , guard
    ]

  -- reachable in 14s, candidate #32747 at size 14.
  -- increase target to 50400 to reach...
  conjureFromSpecWith args{target=5040} "beyond" beyondSpec
    [ pr Leaf
    , prim "Node" Node
    , prim "==" ((==) :: Int -> Int -> Bool)
    , prim "<=" ((<) :: Int -> Int -> Bool)
    , guard
    ]

  -- with 15, this reaches the solution, using 12 for shorter runtime
  -- using maxEquationSize = 7 reduces runtime from 13s to 11s
  conjureFromSpecWith args{maxSize = 12, maxEquationSize = 7} "before" beforeSpec
    [ pr Leaf
    , prim "Node" Node
    , prim "`compare`" (compare :: Int -> Int -> Ordering)
    , primOrdCaseFor (undefined :: Tree)
    ]

  -- with 15, this reaches the solution, using 12 for shorter runtime
  -- using maxEquationSize = 7 reduces runtime from 13s to 11s
  conjureFromSpecWith args{maxSize = 12, maxEquationSize = 7} "beyond" beyondSpec
    [ pr Leaf
    , prim "Node" Node
    , prim "`compare`" (compare :: Int -> Int -> Ordering)
    , primOrdCaseFor (undefined :: Tree)
    ]

  -- reachable in 55s, candidate #173109 at size 13.
  -- increase to 554400 to reach
  conjureWith args{target=5544} "union" union
    [ pr Leaf
    , prim "Node" Node
    , prim "before" before
    , prim "beyond" beyond
    ]
  -- maybe with invariant following test data there will be more pruning
  -- properties?

beforeSpec :: (Int -> Tree -> Tree) -> Bool
beforeSpec before  =  and
  [ holds n $ \x t -> ordered t ==> inorder (before x t) == takeWhile (< x) (inorder t)
  ] where n = 360

beyondSpec :: (Int -> Tree -> Tree) -> Bool
beyondSpec beyond  =  and
  [ holds n $ \x t -> ordered t ==> inorder (beyond x t) == dropWhile (<= x) (inorder t)
  ] where n = 360

-- unionSpec :: (Int -> Tree -> Tree) -> Bool
-- unionSpec union  =  and
--   [ holds n $ \t1 t2 -> ordered t ==> inorder (union t1 t2) == merge (inorder t1) (inorder t2)
--   ] where n = 360

-- same as insert, but using an if instead of a case:
insertIf :: Int -> Tree -> Tree
insertIf x Leaf  =  unit x                                   -- 2
insertIf x (Node l y r)  =  if x == y                        -- 6
                            then Node l y r                  -- 10
                            else if x < y                    -- 14
                                 then Node (insert x l) y r  -- 20
                                 else Node l y (insert x r)  -- 26
