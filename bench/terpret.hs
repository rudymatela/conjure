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


t1p :: [Bool] -> [Bool]
t1p [False]  =  [True]
t1p [True]   =  [False]
t1p [False,True]  =  [True,False]
t1p [True,False]  =  [False,True]

t1g :: [Bool] -> [Bool]
t1g ps  =  map not ps

t1c :: IO ()
t1c  =  do
  putStrLn "TerpreT benchmark #1\n"

  conjure "invert" t1p
    [ pr ([] :: [Bool])
    , prim ":" ((:) :: Bool -> [Bool] -> [Bool])
    , prim "not" (not :: Bool -> Bool)
    ]

  conjure "invert" t1p
    [ prim "not" (not :: Bool -> Bool)
    , prim "map" (map :: (Bool -> Bool) -> [Bool] -> [Bool])
    ]


main :: IO ()
main  =  do
  as <- getArgs
  case as of
    [] -> sequence_ ts
    (n:_) -> ts !! (read n - 1)


ts :: [IO ()]
ts  =  [ t1c
       ]
