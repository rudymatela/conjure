-- dupos.hs: duplicates and positions
--
-- Copyright (C) 2021 Rudy Matela
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
import Conjure
import Test.LeanCheck  -- needed for duplicatesSpec only

duplicates :: [Int] -> [Int] -- Eq a => [a] -> [a]
duplicates []  =  []
duplicates (x:xs)  =
  if x `elem` xs && not (x `elem` d)
  then x : d
  else d
  where
  d  =  duplicates xs

duplicates' :: [Int] -> [Int]
duplicates' [0,0]  =  [0]
duplicates' [0,1]  =  []
duplicates' [1,0,1]  =  [1]
duplicates' [0,1,0,1]  =  [0,1]
duplicates' [1,0,1,0,1]  =  [0,1]
duplicates' [0,1,2,1]  =  [1]

duplicatesSpec :: ([Int] -> [Int]) -> [Bool]
duplicatesSpec duplicates  =  -- TODO: return list of tests instead of holds
  [ holds 360 $ \x xs -> (count (x ==) xs > 1) == elem x (duplicates xs)
  , holds 360 $ \x xs -> count (x ==) (duplicates xs) <= 1
  ]  where  count p  =  length . filter p

positionsFrom :: Int -> Int -> [Int] -> [Int]
positionsFrom n x  =  from n
  where
  from _ []  =  []
  from n (y:ys)  =  if y == x
                    then n : f
                    else f
    where
    f  =  from (n+1) ys

-- this is what conjure _can_ generate
positionsFrom' :: Int -> A -> [A] -> [Int]
positionsFrom' _ _ []      =  []                                  --  1
positionsFrom' n x (y:ys)  =  if y == x                           --  4
                              then n : positionsFrom' (n+1) x ys  -- 12
                              else     positionsFrom' (n+1) x ys  -- 18

main :: IO ()
main = do
  -- duplicates xs  =
  --   if null xs                                                                   --  3
  --   then []                                                                      --  4
  --   else if head xs `elem` tail xs && not (head xs `elem` duplicates (tail xs))  -- 18
  --        then head xs : duplicates (tail xs)                                     -- 24
  --        else duplicates (tail xs)                                               -- 27
  -- out of reach memory and performance wise
  -- -- OR --
  -- duplicates []  =  []                                                  --  1
  -- duplicates (x:xs)  =  if x `elem` xs && not (x `elem` duplicates xs)  -- 11
  --                       then x : duplicates xs                          -- 15
  --                       else duplicates xs                              -- 17
  -- within reach performance wise.
  conjureWith args{maxSize=18} "duplicates" duplicates'
    [ pr ([] :: [Int])
    , prim "not" not
    , prim "&&" (&&)
    , prim ":" ((:) :: Int -> [Int] -> [Int])
    , prim "elem" (elem :: Int -> [Int] -> Bool)
    , prif (undefined :: [Int])
    ]

  conjureFromSpecWith args{maxSize=18} "duplicates" duplicatesSpec
    [ pr ([] :: [Int])
    , prim "not" not
    , prim "&&" (&&)
    , prim ":" ((:) :: Int -> [Int] -> [Int])
    , prim "elem" (elem :: Int -> [Int] -> Bool)
    , prif (undefined :: [Int])
    ]

  -- found!
  conjureWithMaxSize 14 "positionsFrom" positionsFrom'
    [ pr ([] :: [Int])
    , pr (1 :: Int)
    , prim "+" ((+) :: Int -> Int -> Int)
    , prim ":" ((:) :: Int -> [Int] -> [Int])
    , prim "==" ((==) :: A -> A -> Bool)

--  , prif (undefined :: [Int])
    -- cheat codes:
    , prim "id" (id :: [Int] -> [Int])
    , prif (undefined :: [Int] -> [Int])
    ]
