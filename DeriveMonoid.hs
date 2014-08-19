{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving #-}

module DeriveMonoid where

import Language.Haskell.TH
import Control.Applicative
import Control.Monad
import Data.Maybe (catMaybes)
import Data.Monoid (Monoid (..))

lookupT :: Q [Dec]
lookupT = do
  i <- reify ''Monoid
  let str = show i
  [d| result = str |]

deriveMonoid :: Name -> Q [Dec]
deriveMonoid n = do
  info <- reify n
  let str = show info
  case info of
    TyConI d -> case d of
      DataD    _ n tvbs [con] _ -> mkMonoidInstance n tvbs con
      NewtypeD _ n tvbs  con  _ -> mkMonoidInstance n tvbs con
      DataD _ _ _ _ _           -> deriveFail "unsupported number of constructors"
      _                         -> deriveFail "unsupported type"
    _                           ->   deriveFail "not a type"
  where
  deriveFail msg = fail $ unwords ["Can't derive Monoid instance for",show n,";",msg]

extractFromCon :: Con -> Q (Name,[Type])
extractFromCon c = case c of
  NormalC n sts    -> return (n, map fromStrictType sts)
  RecC n vsts      -> return (n, map fromVarStrictType vsts)
  InfixC st1 n st2 -> return (n, [fromStrictType st1,fromStrictType st2])
  ForallC {} -> fail $ unwords ["unsupported constructor for",show c]
  where
  fromStrictType (_,t) = t
  fromVarStrictType (_,_,t) = t

mkMonoidInstance :: Name -> [TyVarBndr] -> Con -> Q [Dec]
mkMonoidInstance n tvbs con = do
  (conNm,fs) <- extractFromCon con
  i <- finishInstance n tvbs conNm fs
  return [i]

finishInstance :: Name -> [TyVarBndr] -> Name -> [Type] -> Q Dec
finishInstance typN tvbs conNm fs = instanceD (context fs) typ [memptyDecl,mappendDecl]
  where
  context fs = cxt $ concat $ map monoidInstance fs
  monoidInstance f = if isGround f
    then []
    else [classP ''Monoid [return f]]
  typ = return $ AppT (ConT ''Monoid) $ foldl AppT (ConT typN) $ map bndrToVar tvbs
  len = length fs

  memptyDecl = funD 'mempty [ clause [] (normalB body) [] ]
    where
    body = return $ foldl AppE (ConE conNm) $ replicate len (VarE 'mempty)

  mappendDecl = do
    vs1 <- replicateM len $ newName "v1"
    vs2 <- replicateM len $ newName "v2"
    funD 'mappend [ clause (pats vs1 vs2) (normalB $ body vs1 vs2) [] ]
    where
    pats vs1 vs2 = [ mkPat vs1 , mkPat vs2 ]
    body vs1 vs2 = return $ foldl AppE (ConE conNm) $ zipWith mkMappend vs1 vs2

    mkMappend v1 v2 = AppE (AppE (VarE 'mappend) (VarE v1)) (VarE v2)
    mkPat vs = return $ ConP conNm $ map VarP vs

bndrToVar :: TyVarBndr -> Type
bndrToVar b = VarT $ case b of
  PlainTV n    -> n
  KindedTV n _ -> n
conName :: Con -> Name
conName c = case c of
  NormalC n _   -> n
  RecC n _      -> n
  InfixC _ n _  -> n
  ForallC _ _ c -> conName c

isGround :: Type -> Bool
isGround t = case t of
  ArrowT     -> True
  ListT      -> True
  ConT   _   -> True
  TupleT _   -> True
  VarT   _   -> False
  AppT t1 t2 -> isGround t1 && isGround t2
  _ -> error "unsupported type"

