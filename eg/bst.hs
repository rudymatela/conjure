-- bst.hs: conjuring functions over binary search trees (BSTs)
--
-- Copyright (C) 2021-2025 Rudy Matela
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
{-# LANGUAGE CPP, TemplateHaskell #-}

import Conjure
import Test.LeanCheck
import Data.Function (on)
import Data.List (nub,sort)

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

inorder :: Tree -> [Int]
inorder Leaf  =  []
inorder (Node l x r)  =  inorder l ++ [x] ++ inorder r

-- membership on a binary search tree
mem :: Int -> Tree -> Bool
mem _ Leaf  =  False
mem x (Node l y r)
  | x < y  =  mem x l
  | y < x  =  mem x r
  | otherwise  =  True

-- this inserts at the leaf
insert :: Int -> Tree -> Tree
insert x Leaf          =  unit x  -- 2
insert x (Node l y r)
  | x < y  =  Node (insert x l) y r  -- 12
  | y < x  =  Node l y (insert x r)  -- 22
  | otherwise  =  Node l y r         -- 25

before :: Int -> Tree -> Tree
before x Leaf  =  Leaf
before x (Node t1 y t2)
  | y < x  =  Node t1 y (before x t2)
  | otherwise  =  before x t1

beyond :: Int -> Tree -> Tree
beyond x Leaf  =  Leaf
beyond x (Node t1 y t2)
  | x < y  =  Node (beyond x t1) y t2
  | otherwise  =  beyond x t2

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

  -- out of reach performance-wise (reaching 19 but needs size 25)
  conjure "insert" insert
    [ fun "Node" Node
    , fun "unit" unit
    , guard
    , fun "<" ((<) :: Int -> Int -> Bool)
    , maxSize 12
    ]

  -- reachable in 7s, candidate #92490 at size 14.
  conjureFromSpec "before" beforeSpec
    [ con Leaf
    , fun "Node" Node
    , fun "==" ((==) :: Int -> Int -> Bool)
    , fun "<" ((<) :: Int -> Int -> Bool)
    , guard
    , target 50400
    ]

  -- reachable in 7s, candidate #92359 at size 14.
  -- increase target to 50400 to reach...
  conjureFromSpec "beyond" beyondSpec
    [ con Leaf
    , fun "Node" Node
    , fun "==" ((==) :: Int -> Int -> Bool)
    , fun "<" ((<) :: Int -> Int -> Bool)
    , guard
    , target 5040
    ]

  conjure "insert" insert
    [ fun "Node" Node
    , fun "before" before
    , fun "beyond" beyond
    ]

  -- reachable in 22s, candidate #233221 at size 13.
  -- -- 21.3s, tested 233221 candidates
  -- union t1 Leaf  =  t1
  -- union t1 (Node t2 x t3)  =  Node (union (before x t1) t2) x (union (beyond x t1) t3)
  --
  -- This needs actual union as spec though... ... test more later
  conjure "union" union
    [ con Leaf
    , fun "Node" Node
    , fun "before" before
    , fun "beyond" beyond
    , target 5544 -- increase to 554400 to reach
    -- , carryOn -- needed for efficient version
    ]

beforeSpec :: (Int -> Tree -> Tree) -> [Property]
beforeSpec before  =
  [ property $ \x t -> inorder (before x t) == takeWhile (< x) (inorder t)
  ]

beyondSpec :: (Int -> Tree -> Tree) -> [Property]
beyondSpec beyond  =
  [ property $ \x t -> inorder (beyond x t) == dropWhile (<= x) (inorder t)
  ]

unionSpec :: (Tree -> Tree -> Tree) -> [Property]
unionSpec union  =
  [ property $ \t1 t2 -> inorder (union t1 t2) == inorder t1 +++ inorder t2
  ]
  where
  xs +++ ys  =  nub . sort $ xs ++ ys  -- nubMerge
