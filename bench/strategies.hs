-- carryon.hs: conjuring implementations of a factorial function
--
-- Copyright (C) 2021-2024 Rudy Matela
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
import Conjure

factorial :: Int -> Int
factorial 1  =  1
factorial 2  =  2
factorial 3  =  6
factorial 4  =  24


mkStrategy :: String -> Args
mkStrategy s  =  args
  { rewriting         =  r
  , requireDescent    =  d
  , adHocRedundancy   =  a
  , copyBindings      =  a -- previously c
  , uniqueCandidates  =  u
--, carryOn  =  True
--, maxSize  =  10
  }
  where
  itob 0  =  False
  itob _  =  True
  [r,d,a,u]  =  map itob $
    case s of
    "unique candidates" -> [1,1,1,1]
    "default"           -> [1,1,1,0]
    "without reasoning" -> [0,1,1,0]
    "without descent"   -> [1,0,1,0]
    "without ad-hoc"    -> [1,1,0,0]
    "only reasoning"    -> [1,0,0,0]
    "only descent"      -> [0,1,0,0]
    "only ad-hoc"       -> [0,0,1,0]
    "only typed"        -> [0,0,0,0]
    _                   -> error "unknown strategy"

conjureStrategy :: Conjurable f => String -> String -> f -> [Prim] -> IO ()
conjureStrategy name nm f prims  =  do
  putStrLn $ "-- strategy: " ++ name
  conjureWith (mkStrategy name) nm f prims

strategies :: [String]
strategies  =  
  [ "unique candidates"
  , "default"
  , "without reasoning"
  , "without descent"
  , "without ad-hoc"
  , "only reasoning"
  , "only descent"
  , "only ad-hoc"
  , "only typed"
  ]

main :: IO ()
main  =  do
  sequence_
    [ conjureStrategy s "factorial n" factorial primitives
    | s <- strategies
    ]

primitives :: [Prim]
primitives = 
    [ pr (0::Int)
    , pr (1::Int)
    , prim "+" ((+) :: Int -> Int -> Int)
    , prim "*" ((*) :: Int -> Int -> Int)
    , prim "-" ((-) :: Int -> Int -> Int)
    ]
