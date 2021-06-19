-- count.hs: elem and set functions
import Conjure

count :: Eq a => a -> [a] -> Int
count x  =  c
  where
  c []  =  0
  c (y:ys)  =  (if x == y then 1 else 0) + c ys

-- this is the expected function
countIf :: A -> [A] -> Int
countIf x xs  =  if null xs                                                 -- 3
                 then 0                                                     -- 4
                 else (if x == head xs then 1 else 0) + countIf x (tail xs) -- 16
                 --    5  6 7   8   9      10     11 12   13   14   15  16

count' :: A -> [A] -> Int
count' 0 [0]  =  1
count' 0 [1]  =  0
count' 1 [0]  =  0
count' 1 [1]  =  1
count' 0 [0,0]  =  2
count' 0 [0,1]  =  1
count' 0 [1,2]  =  0
count' 1 [0,0]  =  0
count' 1 [0,1]  =  1
count' 1 [1,2]  =  1
count' 0 [0,0,0]  =  3
count' 0 [0,0,1]  =  2
count' 0 [1,0,0]  =  2

main :: IO ()
main = do
  -- count x xs  =  length (filter (== x) xs)
  --                1       2       3  4  5
  conjure "count" count'
    [ value "length" (length :: [A] -> Int)
    , value "filter" (filter :: (A -> Bool) -> [A] -> [A])
    , value "==" ((==) :: A -> A -> Bool)
    ]

  conjureWithMaxSize 16 "count" count'
    [ val (0 :: Int)
    , val (1 :: Int)
    , value "+" ((+) :: Int -> Int -> Int)
    , value "head" (head :: [A] -> A)
    , value "tail" (tail :: [A] -> [A])
    , value "null" (null :: [A] -> Bool)
    , value "==" ((==) :: A -> A -> Bool)
    , value "if" (\p x y -> if p then x else y :: Int)
    ]
