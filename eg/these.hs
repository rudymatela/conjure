-- Example
{-# LANGUAGE TemplateHaskell #-}

import Conjure
import Test.LeanCheck


-- This was inspired by the These datatype from the allThis package.
-- https://hackage.haskell.org/package/allThis
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

allThis' :: [These A B] -> [A]
allThis' [This 0, This 1]  =  [0,1]
allThis' [None, That 0]  =  []
allThis' [This 0, None, That 1]  =  [0]
allThis' [This 0, These 0 1]  =  [0,0]
allThis' [These 0 1, None, This 0]  =  [0,0]

allThat' :: [These A B] -> [B]
allThat' [This 0, This 1]  =  []
allThat' [None, That 0]  =  [0]
allThat' [This 0, None, That 1]  =  [1]
allThat' [That 0, These 0 1]  =  [0,1]

these' :: These A A -> [A]
these' (These 1 2)  =  [1,2]
these' (This 1)  =  [1]
these' (That 2)  =  [2]

allThese' :: [These A A] -> [A]
allThese' [This 0, This 1]  =  [0,1]
allThese' [None, That 0]  =  [0]
allThese' [This 0, None, That 1]  =  [0,1]
allThese' [This 0, These 0 1]  =  [0,0,1]
allThese' [These 0 1, That 2]  =  [0,1,2]

main :: IO ()
main  =  do
  conjure "fromThese" fromThese'
    [ prim "," ((,) :: A -> B -> (A,B))
    ]

  conjure "these" these'
    [ pr ([] :: [A])
    , prim ":" ((:) :: A -> [A] -> [A])
    ]

  conjure "allThis" allThis'
    [ pr ([] :: [A])
    , prim ":" ((:) :: A -> [A] -> [A])
    , prim "isThis" (isThis :: These A B -> Bool)
    , prim "fromThis" (fromThis :: These A B -> A)
    , prif (undefined :: [A])
    ]

  conjure "allThat" allThat'
    [ pr ([] :: [B])
    , prim ":" ((:) :: B -> [B] -> [B])
    , prim "isThat" (isThat :: These A B -> Bool)
    , prim "fromThat" (fromThat :: These A B -> B)
    , prif (undefined :: [B])
    ]

  -- couldn't make this reachable, I didn't try much...
  conjure "allThese" allThese'
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
