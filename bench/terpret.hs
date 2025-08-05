-- terpret.hs: Benchmark execution-model problems from TerpreT
--
-- Copyright (C) 2021-2025 Rudy Matela
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
--
-- You can find the description of these exercises on:
--
-- 1. https://dl.acm.org/doi/pdf/10.1145/3067695.3082533
-- 2. https://www.microsoft.com/en-us/research/uploads/prod/2017/03/1608.04428.pdf
--
-- Here, we prefer the simplified definitions found in 1.
import Conjure
import System.Environment (getArgs)


-- TerpreT #1 -- invert --

t1p :: [Bool] -> [Bool]
t1p [False]  =  [True]
t1p [True]   =  [False]
t1p [False,True]  =  [True,False]
t1p [True,False]  =  [False,True]

t1g :: [Bool] -> [Bool]
t1g ps  =  map not ps

t1c :: IO ()
t1c  =  do
  putStrLn "TerpreT benchmark #1: invert\n"

  conjure "invert" t1p ingredients123

-- the same ingredients are used for TerpreT #1, #2 and #3
ingredients123 :: [Ingredient]
ingredients123  =
  [ con False
  , con True
  , fun "not" not
  , fun "&&" (&&)
  , fun "||" (||)
  , con ([] :: [Bool])
  , fun ":" ((:) :: Bool -> [Bool] -> [Bool])
--, fun "map" (map :: (Bool -> Bool) -> [Bool] -> [Bool])
  , guard
  ]


-- TerpreT #2 -- prepend zero --
t2p :: [Bool] -> [Bool]
t2p [True]  =  [False,True]
t2p [False]  =  [False,False]
t2p [True,True]  =  [False,True,True]

t2g :: [Bool] -> [Bool]
t2g ps  =  False:ps

t2c :: IO ()
t2c  =  do
  putStrLn "TerpreT benchmark #2: prepend zero\n"

  conjure "prependZero" t2p ingredients123


-- TerpreT #3 -- binary decrement --
-- here we choose to represent with little-endian notation

t3p :: [Bool] -> [Bool]
-- t3p [True]  =  [False]
t3p [True,True]  =  [False,True]             -- 3-1=2
t3p [False,True]  =  [True,False]            -- 2-1=1
t3p [False,True,True]  =  [True,False,True]  -- 6-1=5

t3g :: [Bool] -> [Bool]
t3g []  =  []
t3g (p:ps)  =  if p
               then False : ps
               else True : t3g ps
-- hah!  Conjure surprised me again:
-- decrement []  =  []
-- decrement (p:ps)  =  not p:(if p then ps else decrement ps)

t3c :: IO ()
t3c  =  do
  putStrLn "TerpreT benchmark #3: binary decrement\n"

  conjure "decrement" t3p ingredients123


-- TerpreT #4 -- 2-bit controlled shift register --

t4p1 :: (Bool,Bool,Bool) -> (Bool,Bool,Bool)
t4p1 (False, True, False)  =  (False, True, False)
t4p1 (False, False, True)  =  (False, False, True)
t4p1 (True, True, False)  =  (True, False, True)
t4p1 (True, False, True)  =  (True, True, False)

t4p2 :: Bool -> Bool -> Bool -> (Bool,Bool,Bool)
t4p2 False  True   False   =  (False, True, False)
t4p2 False  False  True    =  (False, False, True)
t4p2 True   True   False   =  (True, False, True)
t4p2 True   False  True    =  (True, True, False)

t4g :: (Bool,Bool,Bool) -> (Bool,Bool,Bool)
t4g (False,p,q)  =  (False,p,q)
t4g (True,p,q)  =  (True,q,p)

t4c :: IO ()
t4c  =  do
  putStrLn "TerpreT benchmark #4: controlled shift\n"

  conjure "cshift" t4p1 $ ingredients123 ++
    [ fun ",," ((,,) :: Bool -> Bool -> Bool -> (Bool,Bool,Bool))
    , guard
    , target 50400
    ]

  conjure "cshift" t4p2 $ ingredients123 ++
    [ fun ",," ((,,) :: Bool -> Bool -> Bool -> (Bool,Bool,Bool))
    , guard
    ]


-- TerpreT #5 -- Full adder --

t5p :: Bool -> Bool -> Bool -> (Bool,Bool)
t5p False False False  =  (False,False)
t5p False False True   =  (False,True )
t5p False True  False  =  (False,True )
t5p True  False False  =  (False,True )
t5p False True  True   =  (True ,False)
t5p True  False True   =  (True ,False)
t5p True  True  False  =  (True ,False)
t5p True  True  True   =  (True ,True )

t5c :: IO ()
t5c  =  do
  putStrLn "TerpreT benchmark #5: full adder\n"

  conjure "fadder" t5p $ ingredients123 ++
    [ fun "not" not
    , fun "," ((,) :: Bool -> Bool -> (Bool,Bool))
    , fun "==" ((==) :: Bool -> Bool -> Bool)
--  , fun "^^" ((/=) :: Bool -> Bool -> Bool) -- poor man's xor
    , guard
    ]
-- the printed function is weird, but correct
-- fadder p q r  =  if p == q then (p,r) else (r,not r)


-- TerpreT #6 -- 2-bit adder --
t6p :: (Bool,Bool) -> (Bool,Bool) -> (Bool,Bool,Bool)
t6p (False,False) (False,False)  =  (False,False,False)
t6p (True ,False) (False,True )  =  (False,True ,True )
t6p (True ,True ) (False,True )  =  (True ,False,False)
t6p (True ,True ) (True ,False)  =  (True ,False,True )
t6p (True ,True ) (True ,True )  =  (True ,True ,False)

t6c :: IO ()
t6c  =  do
  putStrLn "TerpreT benchmark #6: 2-bit adder\n"
  conjure "adder2" t6p $ ingredients123 ++
    [ fun ",," ((,,) :: Bool -> Bool -> Bool -> (Bool,Bool,Bool))
    , fun "==" ((==) :: Bool -> Bool -> Bool)
    , fun "^^" ((/=) :: Bool -> Bool -> Bool) -- poor man's xor
    , guard
    , maxSize 6
    ]


-- TerpreT #7 -- Access --
t7p :: [A] -> Int -> A
t7p [1,0] 0  =  1
t7p [1,0] 1  =  0
t7p [0,2,1]  0  =  0
t7p [0,2,1]  1  =  2
t7p [0,2,1]  2  =  1

t7g :: [A] -> Int -> A
t7g [] _  =  undefined
t7g (x:xs) 0  =  x
t7g (x:xs) i  =  t7g xs (i-1)

t7c :: IO ()
t7c  =  do
  putStrLn "TerpreT benchmark #7: access\n"
  -- yes, one can implement index with index...
  conjure "`access`" t7p
    [ fun "!!" ((!!) :: [A] -> Int -> A)
    ]

  conjure "`access`" t7p
    [ con (0 :: Int)
    , con (1 :: Int)
    , con ([] :: [A])
    , fun ":" ((:) :: A -> [A] -> [A])
    , fun "-" ((-) :: Int -> Int -> Int)
    , fun "undefined" (undefined :: A)
    ]


-- TerpreT #8 -- Decrement Elements --
t8p :: [Int] -> [Int]
t8p [2]  =  [1]
t8p [1,0]  =  [0,-1]
t8p [0,1,2]  =  [-1,0,1]

t8c :: IO ()
t8c  =  do
  putStrLn "TerpreT benchmark #8: decrement elements\n"

  conjure "decrelements" t8p
    [ con (1 :: Int)
    , con ([] :: [Int])
    , fun ":" ((:) :: Int -> [Int] -> [Int])
    , fun "-" ((-) :: Int -> Int -> Int)
    , fun "map" (map :: (Int -> Int) -> [Int] -> [Int])
    ]
  -- above, even though map is provided, Conjure cannot use it as it cannot
  -- introduce lambdas

  conjure "decrelements" t8p
    [ con (1 :: Int)
    , con ([] :: [Int])
    , fun ":" ((:) :: Int -> [Int] -> [Int])
    , fun "-" ((-) :: Int -> Int -> Int)
    , fun "map" (map :: (Int -> Int) -> [Int] -> [Int])
    , fun "subtract" (subtract :: Int -> Int -> Int)
    ]
  -- now above, the story changes because of subtract, map is used


main :: IO ()
main  =  do
  as <- getArgs
  case as of
    [] -> sequence_ ts
    (n:_) -> ts !! (read n - 1)


ts :: [IO ()]
ts  =  [ t1c
       , t2c
       , t3c
       , t4c
       , t5c
       , t6c
       , t7c
       , t8c
       ]
