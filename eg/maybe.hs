-- maybe.hs: conjuring functions over maybe values
--
-- Copyright (C) 2025 Rudy Matela
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
import Conjure

isNothing' :: Maybe A -> Bool
isNothing' Nothing   =  True
isNothing' (Just 0)  =  False
isNothing' (Just 1)  =  False

isJust' :: Maybe A -> Bool
isJust' Nothing   =  False
isJust' (Just 0)  =  True
isJust' (Just 1)  =  True

fromMaybe' :: A -> Maybe A -> A
fromMaybe' 0 Nothing  =  0
fromMaybe' 1 Nothing  =  1
fromMaybe' 0 (Just 1)  =  1
fromMaybe' 1 (Just 2)  =  2

maybeSpec :: (A -> (A -> A) -> Maybe A -> A) -> Bool
maybeSpec maybe  =  and
  [ maybe 0 undefined Nothing == 0
  , maybe 1 undefined Nothing == 1
  , maybe undefined (+1) (Just 1) == 2
  , maybe undefined (*2) (Just 3) == 6
  ]

listToMaybe' :: [A] -> Maybe A
listToMaybe' []  =  Nothing
listToMaybe' [2]  =  Just 2
listToMaybe' [0,1]  =  Just 0
listToMaybe' [1,0]  =  Just 1

maybeToList' :: Maybe A -> [A]
maybeToList' Nothing   =  []
maybeToList' (Just 0)  =  [0]
maybeToList' (Just 1)  =  [1]
maybeToList' (Just 2)  =  [2]

catMaybes' :: [Maybe A] -> [A]
catMaybes' []  =  []
catMaybes' [Nothing]  =  []
catMaybes' [Just x]  =  [x]
catMaybes' [Just x, Nothing, Just y]  =  [x,y]

main :: IO ()
main = do
  conjure "isNothing"     isNothing'   ingredients
  conjure "isJust"        isJust'      ingredients
  conjure "fromMaybe"     fromMaybe'   ingredients
  conjureFromSpec "maybe" maybeSpec    ingredients
  conjure "listToMaybe"   listToMaybe' ingredients
  conjure "maybeToList"   maybeToList' ingredients

  -- only top-level break downs, so would need moreIngredients
  conjureWith args{maxPatternDepth=2} "catMaybes"     catMaybes'   ingredients
  -- conjure "mapMaybe" mapMaybe' ingredients  -- same

ingredients :: [Ingredient]
ingredients  =
  [ con (Nothing :: Maybe A)
  , fun "Just" (Just :: A -> Maybe A)

  , con False
  , con True

  , con ([] :: [A])
  , fun ":" ((:) :: A -> [A] -> [A])
  ]

{-
moreIngredients :: [Ingredient]
moreIngredients  =  ingredients ++
  [ fun "isNothing" (isNothing :: Maybe A -> Bool)
  , fun "isJust"    (isJust    :: Maybe A -> Bool)
  , fun "fromJust"  (fromJust :: Maybe A -> A)
  , iif (undefined :: A)
  , iif (undefined :: Maybe A)
  , iif (undefined :: [A])
  ]
-}
