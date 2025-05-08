-- tuple.hs: conjuring functions involving tuples
--
-- 2025 Rudy Matela
-- Distributed under a 3-Clause BSD licence (see the file LICENSE).
import Conjure
import Test.LeanCheck


-- First, all functions from Data.Tuple
-- Though their types have been simplified to (A, A),
-- they might as well have had (A, B), (B, A), etc in types.
-- This makes it to easier to use a list of ingredients for everything.

fst' :: (A,A) -> A
fst' (0,1)  =  0
fst' (1,0)  =  1
fst' (0,2)  =  0
fst' (2,1)  =  2

snd' :: (A,A) -> A
snd' (0,1)  =  1
snd' (1,0)  =  0
snd' (0,2)  =  2
snd' (2,1)  =  1

swap' :: (A,A) -> (A,A)
swap' (0,1)  =  (1,0)
swap' (1,0)  =  (0,1)
swap' (2,1)  =  (1,2)
swap' (1,2)  =  (2,1)

type Curry a b c  =  ((a,b) -> c) -> (a -> b -> c)

currySpec :: Curry A A A -> Bool
currySpec curry  =  and
  [ holds n $ \x y -> curry fst x y == x
  , holds n $ \x y -> curry snd x y == y
  , holds n $ \x y -> curry (\(i,j) -> i + j) x y == x + y
  ]
  where
  n = 360

type Uncurry a b c  =  (a -> b -> c) -> ((a,b) -> c)

uncurrySpec :: Uncurry A A A -> Bool
uncurrySpec uncurry  =  and
  [ holds n $ \x y -> uncurry (+) (x,y) == x + y
  , holds n $ \x y -> uncurry (*) (x,y) == x * y
  ]
  where
  n = 360


-- now two functions that are a bit more interesting:

pairwise :: [A] -> [(A,A)]
pairwise [0,1,2,3]  =  [(0,1), (2,3)]
pairwise [0,1,0,1]  =  [(0,1), (0,1)]
pairwise [0,0,0,0,0,0]  =  [(0,0), (0,0), (0,0)]
-- alt:
-- pairwise [x,y]  =  [(x,y)]
-- pairwise [x,y,z,w]  =  [(x,y), (z,w)]
-- notice above even lists are required for the shallow-pattern solution

catpairs :: [(A,A)] -> [A]
catpairs [(x,y)]  =  [x,y]
catpairs [(x,y), (z,w)]  =  [x,y,z,w]

main :: IO ()
main = do
  -- the following 5 are pretty easy to Conjure:
  conjure "fst"  fst'   []
  conjure "snd"  snd'   []
  conjure "swap" swap'  ingredients
  conjureFromSpec "curry"     currySpec ingredients
  conjureFromSpec "uncurry" uncurrySpec ingredients

  -- these are more interesting:
  conjure "pairwise" pairwise ingredients
  conjure "catpairs" catpairs ingredients

  -- by increasing the pattern depth, we find shorter versions:
  conjure "pairwise" pairwise (maxPatternDepth 2:ingredients)
  conjure "catpairs" catpairs (maxPatternDepth 2:ingredients)

ingredients :: [Ingredient]
ingredients  =
  -- pairs
  [ fun "," ((,) :: A -> A -> (A,A))
  , fun "fst" (fst :: (A,A) -> A)
  , fun "snd" (snd :: (A,A) -> A)

  -- lists
  , fun "[]" ([] :: [A])
  , fun ":" ((:) :: A -> [A] -> [A])
  , fun "null" (null :: [A] -> Bool)
  , fun "head" (head :: [A] -> A)
  , fun "tail" (tail :: [A] -> [A])

  -- lists of pairs
  , fun "[]" ([] :: [(A,A)])
  , fun ":" ((:) :: (A,A) -> [(A,A)] -> [(A,A)])

  -- allow guards
  , guard
  ]

-- expected pairwise
-- TODO: this appears in showCandidates but is not selected for some reason...
pw :: [A] -> [(A,A)]
pw []  =  []
pw (x:xs)
  | null xs  =  []
  | otherwise  =  (x,head xs) : pw (tail xs)
