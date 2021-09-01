-- based code sent by Colin Runciman
import Conjure

drop' :: Int -> [a] -> [a]
drop' 0 []     =  []
drop' 1 []     =  []
drop' 0 [x,y]  =  [x,y]
drop' 1 [x,y]  =  [y]
drop' 2 [x,y]  =  []
drop' 3 [x,y]  =  []
drop' 0 [x,y,z]  =  [x,y,z]
drop' 1 [x,y,z]  =  [y,z]
drop' 2 [x,y,z]  =  [z]
drop' 3 [x,y,z]  =  []

take' :: Int -> [a] -> [a]
take' 0 []     =  []
take' 1 []     =  []
take' 0 [x,y]  =  []
take' 1 [x,y]  =  [x]
take' 2 [x,y]  =  [x,y]
take' 3 [x,y]  =  [x,y]

main :: IO ()
main = do
  -- drop 0 []      =  []               -- 1
  -- drop 0 (x:xs)  =  x : xs           -- 4
  -- drop n []      =  []               -- 5
  -- drop n (x:xs)  =  drop (x - 1) xs  -- 10
  conjure "drop" (drop' :: Int -> [A] -> [A])
    [ pr (0 :: Int)
    , pr (1 :: Int)
    , pr ([] :: [A])
    , prim ":" ((:) :: A -> [A] -> [A])
    , prim "-" ((-) :: Int -> Int -> Int)
    ]

  -- take 0 []  =  []                     -- 1
  -- take 0 (x:xs)  =  []                 -- 2
  -- take x []  =  []                     -- 3
  -- take x (y:xs)  =  y:take (x - 1) xs  -- 10
  conjureWithMaxSize 16 "take" (take' :: Int -> [A] -> [A])
    [ pr (0 :: Int)
    , pr (1 :: Int)
    , pr ([] :: [A])
    , prim "||" ((||) :: Bool -> Bool -> Bool)
    , prim "-" ((-) :: Int -> Int -> Int)
    , prim ":" ((:) :: A -> [A] -> [A])
    ]
