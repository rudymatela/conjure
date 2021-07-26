-- longshot.hs: miscellaneous longshots
--
-- Copyright (C) 2021 Rudy Matela
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
import Conjure

sort' :: [Int] -> [Int]
sort' []       =  []
sort' [x]      =  [x]
sort' [x,y]
  | x <= y     =  [x,y]
  | otherwise  =  [y,x]
sort' [x,y,z]
  | x <= y && y <= z  =  [x,y,z]
  | x <= z && z <= y  =  [x,z,y]
  | y <= x && x <= z  =  [y,x,z]
  | y <= z && z <= x  =  [y,z,x]
  | z <= x && x <= y  =  [z,x,y]
  | z <= y && y <= x  =  [z,y,x]


duplicates :: [Int] -> [Int] -- Eq a => [a] -> [a]
duplicates []  =  []
duplicates (x:xs)  =
  if x `elem` xs && not (x `elem` d)
  then x : d
  else d
  where
  d  =  duplicates xs

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
  -- qsort
  -- qsort xs  =  if null xs                                 --  3
  --              then []                                    --  4
  --              else qsort (filter (< head xs) (tail xs))  -- 11
  --                ++ (head xs:[])                          -- 16
  --                ++ qsort (filter (>= head xs) (tail xs)) -- 24
  -- not only this is out of reach performance wise,
  -- but the needed recursive calls will not be enumerated
  -- -- OR --
  -- qsort []  =  []                           -- 1
  -- qsort (x:xs)  =  qsort (filter (x >) xs)  -- 6
  --            ++ (x:qsort (filter (x <=) xs) -- 14
  -- this one is not out of reach performance wise,
  -- but is not generated because of the deconstruction restriction.
  -- The following does generate a correct but inneficient version of qsort.
  conjureWith args{maxSize=14} "qsort" sort'
    [ pr ([] :: [Int])
    , prim ":" ((:) :: Int -> [Int] -> [Int])
    , prim "++" ((++) :: [Int] -> [Int] -> [Int])
    , prim "<=" ((<=) :: Int -> Int -> Bool)
    , prim ">"  ((>)  :: Int -> Int -> Bool)
    , prim "filter" (filter :: (Int -> Bool) -> [Int] -> [Int])
    ]

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
  -- within reach performance wise,
  -- TODO: why is it not being generated?
  conjureWith args{maxSize=10, maxTests=360} "duplicates" duplicates
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
