-- either.hs: conjuring functions over either values
--
-- Copyright (C) 2024 Rudy Matela
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
import Conjure

isLeft' :: Either A A -> Bool
isLeft' (Left 0)  =  True
isLeft' (Left 1)  =  True
isLeft' (Right 0)  =  False
isLeft' (Right 1)  =  False

isRight' :: Either A A -> Bool
isRight' (Left 0)  =  False
isRight' (Left 1)  =  False
isRight' (Right 0)  =  True
isRight' (Right 1)  =  True

fromLeft' :: A -> Either A A -> A
fromLeft' 0 (Left 1)  =  1
fromLeft' 0 (Left 2)  =  2
fromLeft' 1 (Left 0)  =  0
fromLeft' 0 (Right 1)  =  0
fromLeft' 1 (Right 0)  =  1

fromRight' :: A -> Either A A -> A
fromRight' 0 (Left 1)  =  0
fromRight' 0 (Left 2)  =  0
fromRight' 1 (Left 0)  =  1
fromRight' 0 (Right 1)  =  1
fromRight' 1 (Right 0)  =  0

main :: IO ()
main = do
  conjure "isLeft" isLeft' primitives
  conjure "isRight" isRight' primitives
  conjure "fromLeft" fromLeft' primitives
  conjure "fromLeft" fromRight' primitives

primitives :: [Prim]
primitives  =
  [ prim "Left"  (Left :: A -> Either A A)
  , prim "Right" (Right :: A -> Either A A)

  , pr False
  , pr True

  , pr ([] :: [A])
  , prim ":" ((:) :: A -> [A] -> [A])
  ]
