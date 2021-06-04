-- tree.hs: conjuring functions over trees
--
-- Copyright (C) 2021 Rudy Matela
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
import Conjure
import Test.LeanCheck

data Tree  =  Leaf
           |  Node Tree Int Tree
  deriving (Eq, Ord, Show, Read)

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
size Leaf  =  1
size (Node l _ r)  =  size l + 1 + size r

-- TODO: elem and insert


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

  -- TODO: make it so that these are found
  conjure "size" size
    [ val (0 :: Int)
    , val (1 :: Int)
    , value "+" ((+) :: Int -> Int -> Int)
    , value "nil" nil
    , value "left" left
    , value "right" right
    ]

  conjureWithMaxSize 13 "height" height
    [ val (0 :: Int)
    , val (1 :: Int)
    , val (-1 :: Int)
    , value "+" ((+) :: Int -> Int -> Int)
    , value "max" (max :: Int -> Int -> Int)
    , value "nil" nil
    , value "left" left
    , value "right" right
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
