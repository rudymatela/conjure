-- count.hs: conjuring an element count function
--
-- 2021-2025 Rudy Matela
import Conjure

-- an idiomatic count without using filter
count :: Eq a => a -> [a] -> Int
count x  =  c
  where
  c []  =  0
  c (y:ys)  =  (if x == y then 1 else 0) + c ys

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
    [ fun "length" (length :: [A] -> Int)
    , fun "filter" (filter :: (A -> Bool) -> [A] -> [A])
    , fun "==" ((==) :: A -> A -> Bool)
    ]

  -- count x []  =  0
  -- count x (y:xs)  =  count x xs + (if x == y then 1 else 0)
  conjure "count" count'
    [ unfun (0 :: Int)
    , unfun (1 :: Int)
    , fun "+" ((+) :: Int -> Int -> Int)
    , fun "==" ((==) :: A -> A -> Bool)
    , iif (undefined :: Int)
    ]

  -- a little bit larger, guards are only allowed at the root
  -- so there is a need to repeat the recursive call twice
  conjure "count" count'
    [ unfun (0 :: Int)
    , unfun (1 :: Int)
    , fun "+" ((+) :: Int -> Int -> Int)
    , fun "==" ((==) :: A -> A -> Bool)
    , guard
    ]
