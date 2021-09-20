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
import Conjure.Expr hiding (mkName, Name, isInstanceOf)
import Conjure.Conjurable hiding (Name)
import Data.Express.Utils (primeCycle)
import Data.Express.Utils.TH

import Control.Monad
import Data.Char
import Data.List
import Language.Haskell.TH.Lib

-- | Derives an 'Conjurable' instance for the given type 'Name'.
--
-- This function needs the @TemplateHaskell@ extension.
--
-- If '-:', '->:', '->>:', '->>>:', ... are not in scope,
-- this will derive them as well.
deriveConjurable :: Name -> DecsQ
deriveConjurable  =  deriveWhenNeededOrWarn ''Conjurable reallyDerive
  where
  reallyDerive  =  reallyDeriveConjurableWithRequisites

-- | Same as 'deriveConjurable' but does not warn when instance already exists
--   ('deriveConjurable' is preferable).
deriveConjurableIfNeeded :: Name -> DecsQ
deriveConjurableIfNeeded  =  deriveWhenNeeded ''Conjurable reallyDerive
  where
  reallyDerive  =  reallyDeriveConjurableWithRequisites

-- | Derives a 'Conjurable' instance for a given type 'Name'
--   cascading derivation of type arguments as well.
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
  cxt |=>| inst
-- TODO: derive conjureCases
-- TODO: derive conjureSize
-- TODO: derive conjureSubTypes (cf. Extrapolate.subInstances)

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
