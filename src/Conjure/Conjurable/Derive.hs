{-# LANGUAGE TemplateHaskell, CPP #-}
-- |
-- Module      : Conjure.Conjurable.Derive
-- Copyright   : (c) 2019-2021 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- Allows automatic derivation of 'Conjurable' typeclass instances.
module Conjure.Conjurable.Derive
  ( deriveConjurable
  , deriveConjurableCascading
  , deriveConjurableIfNeeded
  )
where

import Test.LeanCheck
import Test.LeanCheck.Derive
import Test.LeanCheck.Utils
import Conjure.Expr hiding (mkName, Name, isInstanceOf)
import Conjure.Conjurable hiding (Name)
import Data.Express.Utils (primeCycle)
import Data.Express.Utils.TH

import Control.Monad
import Data.Char
import Data.List
import Language.Haskell.TH.Lib

#if __GLASGOW_HASKELL__ < 710
import Data.Functor ((<$>))
#endif

-- | Derives an 'Conjurable' instance for the given type 'Name'.
--
-- This function needs the @TemplateHaskell@ extension.
--
-- If '-:', '->:', '->>:', '->>>:', ... are not in scope,
-- this will derive them as well.
--
-- For now,
-- this function only derives
-- 'conjureEquality',
-- 'conjureTiers' and
-- 'conjureExpress'
-- and does not derive
-- 'conjureSubTypes',
-- 'conjureArgumentCases' and
-- 'conjureSize'.
-- These will be added in future versions.
-- If you plan to use features that depend on these functionalities,
-- please define your instances manually.
deriveConjurable :: Name -> DecsQ
deriveConjurable  =  deriveWhenNeededOrWarn ''Conjurable reallyDerive
  where
  reallyDerive  =  reallyDeriveConjurableWithRequisites

-- | Same as 'deriveConjurable' but does not warn when instance already exists
--   ('deriveConjurable' is preferable).
--
-- For now,
-- this function only derives
-- 'conjureEquality',
-- 'conjureTiers' and
-- 'conjureExpress'
-- and does not derive
-- 'conjureSubTypes',
-- 'conjureArgumentCases' and
-- 'conjureSize'.
-- These will be added in future versions.
-- If you plan to use features that depend on these functionalities,
-- please define your instances manually.
deriveConjurableIfNeeded :: Name -> DecsQ
deriveConjurableIfNeeded  =  deriveWhenNeeded ''Conjurable reallyDerive
  where
  reallyDerive  =  reallyDeriveConjurableWithRequisites

-- | Derives a 'Conjurable' instance for a given type 'Name'
--   cascading derivation of type arguments as well.
--
-- For now,
-- this function only derives
-- 'conjureEquality',
-- 'conjureTiers' and
-- 'conjureExpress'
-- and does not derive
-- 'conjureSubTypes',
-- 'conjureArgumentCases' and
-- 'conjureSize'.
-- These will be added in future versions.
-- If you plan to use features that depend on these functionalities,
-- please define your instances manually.
deriveConjurableCascading :: Name -> DecsQ
deriveConjurableCascading  =  deriveWhenNeeded ''Conjurable reallyDerive
  where
  reallyDerive t  =  concat
                 <$> sequence [ deriveListableCascading t
                              , deriveNameCascading t
                              , deriveExpressCascading t
                              , reallyDeriveConjurableCascading t ]

reallyDeriveConjurableWithRequisites :: Name -> DecsQ
reallyDeriveConjurableWithRequisites t  =  concat <$>
  sequence [ deriveListableIfNeeded t
           , deriveNameIfNeeded t
           , deriveExpressIfNeeded t
           , reallyDeriveConjurable t ]

reallyDeriveConjurable :: Name -> DecsQ
reallyDeriveConjurable t  =  do
  isEq <- t `isInstanceOf` ''Eq
  isOrd <- t `isInstanceOf` ''Ord
  (nt,vs) <- normalizeType t
#if __GLASGOW_HASKELL__ >= 710
  cxt <- sequence [ [t| $(conT c) $(return v) |]
#else
  -- template-haskell <= 2.9.0.0:
  cxt <- sequence [ classP c [return v]
#endif
                  | c <- [''Conjurable, ''Listable, ''Express] ++ [''Eq | isEq] ++ [''Ord | isOrd]
                  , v <- vs]
  cs <- typeConstructorsArgNames t
  asName <- newName "x"
  let withTheReturnTypeOfs = deriveWithTheReturnTypeOfs $ [length ns | (_,ns) <- cs]
  let inst = [d| instance Conjurable $(return nt) where
                   conjureExpress   =  reifyExpress
                   conjureEquality  =  reifyEquality
                   conjureTiers     =  reifyTiers |]
  -- withTheReturnTypeOfs |++| (cxt |=>| inst)
  cxt |=>| inst `addFun` deriveSize t `mergeI` deriveSubTypes t `mergeI` deriveCases t
-- TODO: derive conjureCases, e.g.:
-- conjureCases mx  =  [ value "Nothing" (Nothing -: mx)
--                     , value "Just" (Just ->: mx) :$ hole x
--                     ]

deriveCases :: Name -> DecsQ
deriveCases t  =  do
  n <- newName "x"
  (nt,vs) <- normalizeType t
  cs <- typeConstructorsArgNames t
  let lets = [letin n c ns | (c,ns) <- cs]
  let rhs = foldr (\e1 e2 -> [| $e1 : $e2 |]) [|[]|] lets
  [d| instance Conjurable $(return nt) where
        conjureCases $(varP n) = $rhs |]
  where
  letin :: Name -> Name -> [Name] -> ExpQ
  letin x c ns = do
    und <- VarE <$> lookupValN "undefined"
    let lhs = conP c (map varP ns)
    let rhs = return $ foldl AppE (ConE c) [und | _ <- ns]
    let retTypeOf = varE $ mkName $ "-" ++ replicate (length ns) '>' ++ ":"
    let ins = foldl (\e1 e2 -> [| $e1 :$ $e2 |])
                [| value $(stringE $ nameBase c) ($retTypeOf $(conE c) $(varE x)) |]
                [ [| hole $(varE n) |] | n <- ns ]
    [| let $lhs = $rhs `asTypeOf` $(varE x) in $ins |]

deriveSubTypes :: Name -> DecsQ
deriveSubTypes t  =  do
  n <- newName "x"
  (nt,vs) <- normalizeType t
  cs <- typeConstructorsArgNames t
  let lets = [letin n c ns | (c,ns) <- cs, not (null ns)]
  let rhs = foldr0 (\e1 e2 -> [| $e1 . $e2 |]) [|id|] lets
  [d| instance Conjurable $(return nt) where
        conjureSubTypes $(varP n) = $rhs |]
  where
  letin :: Name -> Name -> [Name] -> ExpQ
  letin x c ns = do
    und <- VarE <$> lookupValN "undefined"
    let lhs = conP c (map varP ns)
    let rhs = return $ foldl AppE (ConE c) [und | _ <- ns]
    let bot = foldl1 (\e1 e2 -> [| $e1 . $e2 |])
                     [ [| conjureType $(varE n) |] | n <- ns ]
    [| let $lhs = $rhs `asTypeOf` $(varE x) in $bot |]

deriveSize :: Name -> DecsQ
deriveSize t  =  ((:[]) . FunD (mkName "conjureSize")) <$> deriveSizeClauses t

deriveSizeClauses :: Name -> Q [Clause]
deriveSizeClauses t  =  mapM (uncurry mkClause) =<< typeConstructors t
  where
  mkClause :: Name -> [Type] -> Q Clause
  mkClause n as  =  clause pat body []
    where
    ns  =  take (length as) $ map mkName (variableNamesFromTemplate "x")
    pat  =  [conP n [varP n | n <- ns]]
    body  =  normalB $ foldl (\e n -> [| $e + conjureSize $(varE n) |]) [| 1 |] ns

-- Not only really derive Conjurable instances,
-- but cascade through argument types.
reallyDeriveConjurableCascading :: Name -> DecsQ
reallyDeriveConjurableCascading  =  reallyDeriveCascading ''Conjurable reallyDeriveConjurable

deriveWithTheReturnTypeOfs :: [Int] -> DecsQ
deriveWithTheReturnTypeOfs  =
  fmap concat . mapM deriveWithTheReturnTypeOf . nubSort

deriveWithTheReturnTypeOf :: Int -> DecsQ
deriveWithTheReturnTypeOf n  =  do
  mf <- lookupValueName name
  case mf of
    Nothing -> reallyDeriveWithTheReturnTypeOf n
    Just _  -> return []
  where
  name  =  "-" ++ replicate n '>' ++ ":"

reallyDeriveWithTheReturnTypeOf :: Int -> DecsQ
reallyDeriveWithTheReturnTypeOf n  =  do
  td <- sigD name theT
  vd <- [d| $(varP name) = const |]
  return $ td:vd
  where
  theT  =  bind [t| $(theFunT) -> $(last vars) -> $(theFunT) |]
  theFunT  =  foldr1 funT vars
  funT t1 t2  =  [t| $(t1) -> $(t2) |]
  vars  =  map (varT . mkName) . take (n+1) . primeCycle $ map (:"") ['a'..'z']
  name  =  mkName $ "-" ++ replicate n '>' ++ ":"
#if __GLASGOW_HASKELL__ >= 800
  bind  =  id -- unbound variables are automatically bound
#else
  bind  =  toBoundedQ
#endif

addFun :: DecsQ -> DecsQ -> DecsQ
qds1 `addFun` qds2 = do ds1 <- qds1
                        ds2 <- qds2
                        return $ ds1 `m` ds2
  where
#if __GLASGOW_HASKELL__ < 800
  [InstanceD   c ts ds1] `m` ds2 = [InstanceD   c ts (ds1 ++ ds2)]
#else
  [InstanceD o c ts ds1] `m` ds2 = [InstanceD o c ts (ds1 ++ ds2)]
#endif
