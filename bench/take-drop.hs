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
    [ val (0 :: Int)
    , value "null" (null :: [A] -> Bool)
    , value "==" ((==) :: Int -> Int -> Bool)
    , value "||" (||)
    , value "dec" (subtract 1 :: Int -> Int)
    , value "tail" (tail :: [A] -> [A])
    ]

  -- take n xs = if n==0 || null xs then [] else head xs : take (dec n) (tail xs)
  -- needs size 16
  conjureWithMaxSize 13 "take" (take' :: Int -> [A] -> [A])
    [ val (0 :: Int)
    , val ([] :: [A])
    , value "null" (null :: [A] -> Bool)
    , value "==" ((==) :: Int -> Int -> Bool)
    , value "||" ((||) :: Bool -> Bool -> Bool)
    , value "dec" ((\n -> n-1) :: Int -> Int)
    , value ":" ((:) :: A -> [A] -> [A])
    , value "head" (head :: [A] -> A)
    , value "tail" (tail :: [A] -> [A])
    ]
