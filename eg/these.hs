-- Example
{-# LANGUAGE TemplateHaskell #-}

import Conjure


-- This was inspired by the These datatype from the cathis package.
-- https://hackage.haskell.org/package/cathis
-- This is an extension of the type there with the added Neither constructor.
data These a b  =  Neither | This a | That b | These a b
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
fromThese' 0 1 Neither  =  (0, 1)
fromThese' 0 1 (This 2)  =  (2,1)
fromThese' 0 1 (That 2)  =  (0,2)
fromThese' 0 1 (These 1 0)  =  (1,0)

listhese' :: These A A -> [A]
listhese' (These 1 2)  =  [1,2]
listhese' (This 1)  =  [1]
listhese' (That 2)  =  [2]

cathis' :: [These A B] -> [A]
cathis' []  =  []
cathis' [This 0, This 1]  =  [0,1]
cathis' [Neither, That 0]  =  []
cathis' [That 0, This 1]  =  [1]
cathis' [This 0, Neither, That 1]  =  [0]
cathis' [This 0, These 0 1]  =  [0,0]
cathis' [These 0 1, Neither, This 0]  =  [0,0]
cathis' [These 0 1, This 2]  =  [0,2]

cathat' :: [These A B] -> [B]
cathat' []  =  []
cathat' [This 0, This 1]  =  []
cathat' [Neither, That 0]  =  [0]
cathat' [This 0, Neither, That 1]  =  [1]
cathat' [That 0, These 0 1]  =  [0,1]
cathat' [These 0 1, That 0]  =  [1,0]

cathese' :: [These A A] -> [A]
cathese' []  =  []
cathese' [This 0, This 1]  =  [0,1]
cathese' [Neither, That 0]  =  [0]
cathese' [That 0, This 1]  =  [0,1]
cathese' [This 0, Neither, That 1]  =  [0,1]
cathese' [This 0, These 0 1]  =  [0,0,1]
cathese' [These 0 1, That 2]  =  [0,1,2]

main :: IO ()
main  =  do
  conjure "fromThese" fromThese'
    [ fun "," ((,) :: A -> B -> (A,B))
    ]

  conjure "listhese" listhese'
    [ con ([] :: [A])
    , fun ":" ((:) :: A -> [A] -> [A])
    ]

  conjure "cathis" cathis'
    [ con ([] :: [A])
    , fun ":" ((:) :: A -> [A] -> [A])
    , fun "isThis" (isThis :: These A B -> Bool)
    , fun "fromThis" (fromThis :: These A B -> A)
    , guard
    ]

  conjure "cathat" cathat'
    [ con ([] :: [B])
    , fun ":" ((:) :: B -> [B] -> [B])
    , fun "isThat" (isThat :: These A B -> Bool)
    , fun "fromThat" (fromThat :: These A B -> B)
    , guard
    ]

  conjure "cathis" cathis'
    [ con ([] :: [A])
    , fun ":" ((:) :: A -> [A] -> [A])
    , maxPatternDepth 2
    ]

  conjure "cathat" cathat'
    [ con ([] :: [B])
    , fun ":" ((:) :: B -> [B] -> [B])
    , maxPatternDepth 2
    ]

  conjure "cathese" cathese'
    [ con ([] :: [A])
    , fun ":" ((:) :: A -> [A] -> [A])
    , maxPatternDepth 2
    ]
  -- cathese []  =  []
  -- cathese (Neither : ts)  =  cathese ts
  -- cathese (This x : ts)  =  x : these ts
  -- cathese (That y : ts)  =  y : these ts
  -- cathese (These x y : ts)  =  x : y : these ts
