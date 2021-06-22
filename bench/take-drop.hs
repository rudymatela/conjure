-- based code sent by Colin Runciman
import Conjure

drop' :: Int -> [a] -> [a]
drop' 0 []     =  []
drop' 1 []     =  []
drop' 0 [x,y]  =  [x,y]
drop' 1 [x,y]  =  [y]
drop' 2 [x,y]  =  []
drop' 3 [x,y]  =  []

take' :: Int -> [a] -> [a]
take' 0 []     =  []
take' 1 []     =  []
take' 0 [x,y]  =  []
take' 1 [x,y]  =  [x]
take' 2 [x,y]  =  [x,y]
take' 3 [x,y]  =  [x,y]

main :: IO ()
main = do
  -- drop n xs = if n==0 || null xs then xs else drop (dec n) (tail xs)
  -- needs size 13
  conjureWithMaxSize 13 "drop" (drop' :: Int -> [A] -> [A])
    [ pr (0 :: Int)
    , prim "null" (null :: [A] -> Bool)
    , prim "==" ((==) :: Int -> Int -> Bool)
    , prim "||" (||)
    , prim "dec" (subtract 1 :: Int -> Int)
    , prim "tail" (tail :: [A] -> [A])
    ]

  -- take n xs = if n==0 || null xs then [] else head xs : take (dec n) (tail xs)
  -- needs size 16
  conjureWithMaxSize 16 "take" (take' :: Int -> [A] -> [A])
    [ pr (0 :: Int)
    , pr ([] :: [A])
    , prim "null" (null :: [A] -> Bool)
    , prim "==" ((==) :: Int -> Int -> Bool)
    , prim "||" ((||) :: Bool -> Bool -> Bool)
    , prim "dec" ((\n -> n-1) :: Int -> Int)
    , prim ":" ((:) :: A -> [A] -> [A])
    , prim "head" (head :: [A] -> A)
    , prim "tail" (tail :: [A] -> [A])
    ]
