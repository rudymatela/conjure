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
take' 0 [x]    =  []
take' 1 [x]    =  [x]
take' 0 [x,y]  =  []
take' 1 [x,y]  =  [x]
take' 2 [x,y]  =  [x,y]
take' 3 [x,y]  =  [x,y]

main :: IO ()
main = do
  -- drop 0 xs  =  xs                   -- 1
  -- drop x []  =  []                   -- 2
  -- drop x (y:xs)  =  drop (x - 1) xs  -- 7
  conjure "drop" (drop' :: Int -> [A] -> [A])
    [ pr (0 :: Int)
    , pr (1 :: Int)
    , pr ([] :: [A])
    , prim ":" ((:) :: A -> [A] -> [A])
    , prim "-" ((-) :: Int -> Int -> Int)
    ]

  -- take 0 xs  =  []                     -- 1
  -- take x []  =  []                     -- 2
  -- take x (y:xs)  =  y:take (x - 1) xs  -- 9
  conjure "take" (take' :: Int -> [A] -> [A])
    [ pr (0 :: Int)
    , pr (1 :: Int)
    , pr ([] :: [A])
    , prim "-" ((-) :: Int -> Int -> Int)
    , prim ":" ((:) :: A -> [A] -> [A])
    ]
