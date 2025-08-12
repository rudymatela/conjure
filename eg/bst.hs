-- bst.hs: conjuring functions over binary search trees (BSTs)
--
-- Copyright (C) 2021-2025 Rudy Matela
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
{-# LANGUAGE CPP, TemplateHaskell #-}

import Conjure
import Test.LeanCheck
import Data.Function (on)
import Data.Express hiding (height,size)

data Tree  =  Leaf
           |  Node Tree Int Tree
  deriving (Show, Read)

instance Eq Tree where
  (==)  =  (==) `on` inorder

instance Ord Tree where
  compare  =  compare `on` inorder

unit :: Int -> Tree
unit x  =  Node Leaf x Leaf

isLeaf :: Tree -> Bool
isLeaf Leaf  =  True
isLeaf _     =  False

leftmost :: Tree -> Int
leftmost (Node l x _)
  | isLeaf l  =  x
  | otherwise  =  leftmost l

rightmost :: Tree -> Int
rightmost (Node _ x r)
  | isLeaf r  =  x
  | otherwise  =  rightmost r

size :: Tree -> Int
size Leaf  =  0
size (Node l _ r)  =  size l + 1 + size r

inorder :: Tree -> [Int]
inorder Leaf  =  []
inorder (Node l x r)  =  inorder l ++ [x] ++ inorder r


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

insertAlt :: Int -> Tree -> Tree
insertAlt x Leaf          =  unit x  -- 2
insertAlt x (Node l y r)
  | x < y  =  Node (insert x l) y r  -- 12
  | y < x  =  Node l y (insert x r)  -- 22
  | otherwise  =  Node l y r         -- 25

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
         \/ cons3 Node `suchThat` (\(Node l x r) -> (isLeaf l || rightmost l < x)
                                                 && (isLeaf r || x < leftmost r))

instance Name Tree where
  name _  =  "t1"

deriveConjurable ''Tree


main :: IO ()
main = do
  conjure "mem" mem
    [ con False
    , con True
    , fun "<" ((<) :: Int -> Int -> Bool)
    , guard
    ]

  conjure "mem" mem
    [ con False
    , con True
    , fun "`compare`" (compare :: Int -> Int -> Ordering)
    , ordcase (undefined :: Bool)
    ]

  -- out of reach performance-wise as well (reaching 19 but needs size 25)
  conjure "insert" insert
    [ fun "Node" Node
    , fun "unit" unit
    , guard
    , fun "<" ((<) :: Int -> Int -> Bool)
    , maxSize 12
    ]

  -- simply out of reach performance-wise (reaching 16 but need size 22)
  conjure "insert" insert
    [ fun "Node" Node
    , fun "unit" unit
    , fun "`compare`" (compare :: Int -> Int -> Ordering)
    , ordcase (undefined :: Tree)
    , maxSize 12
    ]

  -- reachable in 15s, candidate #32878 at size 14.
  -- increase target to 50400 to reach...
  conjureFromSpec "before" beforeSpec
    [ con Leaf
    , fun "Node" Node
    , fun "==" ((==) :: Int -> Int -> Bool)
    , fun "<" ((<) :: Int -> Int -> Bool)
    , guard
    , target 5040
    ]

  -- reachable in 14s, candidate #32747 at size 14.
  -- increase target to 50400 to reach...
  conjureFromSpec "beyond" beyondSpec
    [ con Leaf
    , fun "Node" Node
    , fun "==" ((==) :: Int -> Int -> Bool)
    , fun "<" ((<) :: Int -> Int -> Bool)
    , guard
    , target 5040
    ]

  -- with 15, this reaches the solution, using 12 for shorter runtime
  -- using maxEquationSize = 7 reduces runtime from 13s to 11s
  conjureFromSpec "before" beforeSpec
    [ con Leaf
    , fun "Node" Node
    , fun "`compare`" (compare :: Int -> Int -> Ordering)
    , ordcase (undefined :: Tree)
    , maxSize 12
    , maxEquationSize 7
    ]

  -- with 15, this reaches the solution, using 12 for shorter runtime
  -- using maxEquationSize = 7 reduces runtime from 13s to 11s
  conjureFromSpec "beyond" beyondSpec
    [ con Leaf
    , fun "Node" Node
    , fun "`compare`" (compare :: Int -> Int -> Ordering)
    , ordcase (undefined :: Tree)
    , maxSize 12
    , maxEquationSize 7
    ]

  conjure "insert" insert
    [ fun "Node" Node
    , fun "before" before
    , fun "beyond" beyond
    ]

  -- reachable in 55s, candidate #173109 at size 13.
  conjure "union" union
    [ con Leaf
    , fun "Node" Node
    , fun "before" before
    , fun "beyond" beyond
    , target 5544 -- increase to 554400 to reach
    ]
  -- maybe with invariant following test data there will be more pruning
  -- properties?

beforeSpec :: (Int -> Tree -> Tree) -> [Property]
beforeSpec before  =
  [ property $ \x t -> inorder (before x t) == takeWhile (< x) (inorder t)
  ]

beyondSpec :: (Int -> Tree -> Tree) -> [Property]
beyondSpec beyond  =
  [ property $ \x t -> inorder (beyond x t) == dropWhile (<= x) (inorder t)
  ]

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
