module Conjure.Reason
  ( Thy
  , rules
  , equations
  , invalid
  , canReduceTo
  , printThy
  , closureLimit
  , doubleCheck
  , normalize
  , theoryFromAtoms
  , isRootNormalC
  , isRootNormalE
  , isCommutative
  , commutativeOperators
  )
where

import Test.Speculate.Reason
  ( Thy
  , rules
  , equations
  , invalid
  , canReduceTo
  , printThy
  , closureLimit
  , doubleCheck
  , normalize
  )
import Test.Speculate.Engine (theoryFromAtoms)
import Conjure.Expr
import qualified Data.Express.Triexpr as T

--- normality checks ---

isRootNormal :: Thy -> Expr -> Bool
isRootNormal thy e  =  null $ T.lookup e trie
  where
  trie  =  T.fromList (rules thy)

-- the logic of this function is a bit twisted for performance
-- but nevertheless correct
isRootNormalC :: Thy -> Expr -> Bool
isRootNormalC thy e | not (isRootNormal thy e)  =  False
isRootNormalC thy (ef :$ ex :$ ey)
  | ex <= ey  =  True
  | not (isCommutative thy ef)  =  True
  | isRootNormal thy (ef :$ ey :$ ex)  =  False
isRootNormalC _ _  =  True

isRootNormalE :: Thy -> Expr -> Bool
isRootNormalE thy e  =  isRootNormal thy e
                    &&  null (filter (e ->-) [e2 //- bs | (_,bs,e2) <- T.lookup e trie])
  where
  trie  =  T.fromList $ equations thy ++ map swap (equations thy)
  (->-)  =  canReduceTo thy

isCommutative :: Thy -> Expr -> Bool
isCommutative thy eo  =  eo `elem` commutativeOperators thy

commutativeOperators :: Thy -> [Expr]
commutativeOperators thy  =  [ ef
                             | (ef :$ ex :$ ey, ef' :$ ey' :$ ex') <- equations thy
                             , ef == ef'
                             , ex == ex'
                             , ey == ey'
                             ]
