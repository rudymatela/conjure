-- strategies.hs: conjuring factorial with different strategies
--
-- Copyright (C) 2021-2025 Rudy Matela
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
import Conjure

factorial :: Int -> Int
factorial 1  =  1
factorial 2  =  2
factorial 3  =  6
factorial 4  =  24


-- TODO: simplify and refactor
mkStrategy :: String -> [Ingredient]
mkStrategy s  =  [ dontRewrite | not r ]
              ++ [ dontRequireDescent | not d ]
              ++ [ omitAssortedPruning | not a ]
              ++ [ dontCopyBindings | not a ]
              ++ [ uniqueCandidates | u]
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

conjureStrategy :: Conjurable f => String -> String -> f -> [Ingredient] -> IO ()
conjureStrategy name nm f ingrs =  do
  putStrLn $ "-- strategy: " ++ name
  conjure nm f $ ingrs ++ mkStrategy name

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
    [ conjureStrategy s "factorial n" factorial ingredients
    | s <- strategies
    ]

ingredients :: [Ingredient]
ingredients = 
    [ unfun (0::Int)
    , unfun (1::Int)
    , fun "+" ((+) :: Int -> Int -> Int)
    , fun "*" ((*) :: Int -> Int -> Int)
    , fun "-" ((-) :: Int -> Int -> Int)
    ]
