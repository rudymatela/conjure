-- terpret.hs: Benchmark execution-model problems from TerpreT
--
-- Copyright (C) 2021 Rudy Matela
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

  conjure "invert" t1p primitives123

-- the same primitives are used for TerpreT #1, #2 and #3
primitives123 :: [Prim]
primitives123  =
  [ pr False
  , pr True
  , prim "not" not
  , prim "&&" (&&)
  , prim "||" (||)
  , pr ([] :: [Bool])
  , prim ":" ((:) :: Bool -> [Bool] -> [Bool])
--, prim "map" (map :: (Bool -> Bool) -> [Bool] -> [Bool])
  , prif (undefined :: [Bool])
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

  conjure "prependZero" t2p primitives123


-- TerpreT #3 -- binary decrement --
-- here we choose to represent with little-endian notation

t3p :: [Bool] -> [Bool]
-- t3p [True]  =  [False]
t3p [True,True]  =  [False,True]             --  11 - 1 =  10
t3p [False,True]  =  [True,False]            --  10 - 1 =   1
t3p [False,True,True]  =  [True,False,True]  -- 110 - 1 = 101

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

  conjure "decrement" t3p primitives123


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

  conjure "cshift" t4p1 $ primitives123 ++
    [ prim ",," ((,,) :: Bool -> Bool -> Bool -> (Bool,Bool,Bool))
    , prif (undefined :: (Bool,Bool,Bool))
    ]

  conjure "cshift" t4p2 $ primitives123 ++
    [ prim ",," ((,,) :: Bool -> Bool -> Bool -> (Bool,Bool,Bool))
    , prif (undefined :: (Bool,Bool,Bool))
    ]


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
       ]
