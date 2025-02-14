-- Example
{-# LANGUAGE TemplateHaskell #-}

import Conjure
import Test.LeanCheck


-- This was inspired by the These datatype from the cathis package.
-- https://hackage.haskell.org/package/cathis
-- This is an extension of the type there with the added None constructor.
data These a b  =  None | This a | That b | These a b
  deriving (Eq, Ord, Show, Read)

isThis :: These a b -> Bool
isThis (This _)  =  True
isThis (These _ _)  =  True
isThis _  =  False

fromThis :: These a b -> a
fromThis (This x)  =  x
fromThis (These x _)  =  x

isThat :: These a b -> Bool
isThat (That _)  =  True
isThat (These _ _)  =  True
isThat _  =  False

fromThat :: These a b -> b
fromThat (That y)  =  y
fromThat (These _ y)  =  y


deriveConjurable ''These


fromThese' :: A -> B -> These A B -> (A, B)
fromThese' 0 1 None  =  (0, 1)
fromThese' 0 1 (This 2)  =  (2,1)
fromThese' 0 1 (That 2)  =  (0,2)
fromThese' 0 1 (These 1 0)  =  (1,0)

listhese' :: These A A -> [A]
listhese' (These 1 2)  =  [1,2]
listhese' (This 1)  =  [1]
listhese' (That 2)  =  [2]

cathis' :: [These A B] -> [A]
cathis' [This 0, This 1]  =  [0,1]
cathis' [None, That 0]  =  []
cathis' [This 0, None, That 1]  =  [0]
cathis' [This 0, These 0 1]  =  [0,0]
cathis' [These 0 1, None, This 0]  =  [0,0]

cathat' :: [These A B] -> [B]
cathat' [This 0, This 1]  =  []
cathat' [None, That 0]  =  [0]
cathat' [This 0, None, That 1]  =  [1]
cathat' [That 0, These 0 1]  =  [0,1]

cathese' :: [These A A] -> [A]
cathese' [This 0, This 1]  =  [0,1]
cathese' [None, That 0]  =  [0]
cathese' [This 0, None, That 1]  =  [0,1]
cathese' [This 0, These 0 1]  =  [0,0,1]
cathese' [These 0 1, That 2]  =  [0,1,2]

main :: IO ()
main  =  do
  conjure "fromThese" fromThese'
    [ prim "," ((,) :: A -> B -> (A,B))
    ]

  conjure "listhese" listhese'
    [ pr ([] :: [A])
    , prim ":" ((:) :: A -> [A] -> [A])
    ]

  conjure "cathis" cathis'
    [ pr ([] :: [A])
    , prim ":" ((:) :: A -> [A] -> [A])
    , prim "isThis" (isThis :: These A B -> Bool)
    , prim "fromThis" (fromThis :: These A B -> A)
    , prif (undefined :: [A])
    ]

  conjure "cathat" cathat'
    [ pr ([] :: [B])
    , prim ":" ((:) :: B -> [B] -> [B])
    , prim "isThat" (isThat :: These A B -> Bool)
    , prim "fromThat" (fromThat :: These A B -> B)
    , prif (undefined :: [B])
    ]

  -- couldn't make this reachable, I didn't try much...
  conjure "cathese" cathese'
    [ pr ([] :: [A])
    , prim ":" ((:) :: A -> [A] -> [A])
    , prim "++" ((++) :: [A] -> [A] -> [A])
    , prim "isThis" (isThis :: These A A -> Bool)
    , prim "fromThis" (fromThis :: These A A -> A)
    , prim "isThat" (isThat :: These A A -> Bool)
    , prim "fromThat" (fromThat :: These A A -> A)
    -- , prif (undefined :: A)
    , prif (undefined :: [A])
    ]
  -- expected functionality
  -- these []  =  []
  -- these (This x : ts)  =  x : these ts
  -- these (That y : ts)  =  y : these ts
  -- these (These x y : ts)  =  x : y : these ts
