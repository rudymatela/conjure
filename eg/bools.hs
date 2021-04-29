-- bools.hs: simple recursive functions over boolean lists
--
-- Copyright (C) 2021 Rudy Matela
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
import Conjure

main :: IO ()
main = do
  conjure "and" (and :: [Bool] -> Bool) background
  conjure "or"  (or  :: [Bool] -> Bool) background

background :: [Expr]
background =
  [ val False
  , val True
  , value "not" not
  , value "||" (||)
  , value "&&" (&&)
  , value "null" (null :: [Bool] -> Bool)
  , value "head" (head :: [Bool] -> Bool)
  , value "tail" (tail :: [Bool] -> [Bool])
  , ifFor (undefined :: Bool)
  ]

-- target (for and):

-- and ps  =  if null ps then True else head ps && and (tail ps)
--            1  2    3       4         5    6  7  8    9    10 symbols
-- or
-- and ps  =  null ps || head ps && and (tail ps)
--            1    2  3  4    5  6  7    8    9 symbols
