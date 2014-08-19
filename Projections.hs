{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}

module Projections
  ( makeProjections
  , makeProjectionsWith
  ) where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import Control.Arrow ((***))
import Control.Monad (forM, replicateM)

makeProjections :: Name -> Q [Dec]
makeProjections = makeProjectionsWith "project"

makeProjectionsWith :: String -> Name -> Q [Dec]
makeProjectionsWith prefix n = do
  info <- reify n
  maybeT <- [t| Maybe |]
  let (typNm,cs) = getCons info
      cs' = map getFields cs
      cts = map (alterName prefix *** getResultType maybeT typNm) cs'
      typeDs = map (uncurry SigD) cts
  funDs <- forM (zip cs' cts) $ \((cNm,cTs),(funNm,_)) -> do
    as <- replicateM (length cTs) (newName "arg")
    if length cs' == 1
    then projectSingle funNm cNm as
    else do
      a <- newName "arg"
      projectMulti funNm cNm a as
  return (typeDs ++ funDs)

-- Data gatherers {{{

getFields :: Con -> (Name,[Type])
getFields = \case
  NormalC nm ts -> (nm,map snd ts)
  RecC nm vts -> (nm,map thd vts)
  InfixC t1 nm t2 -> (nm,[snd t1,snd t2])
  ForallC {} -> error "Unsupported constructor"
  where
  thd (_,_,x) = x

getResultType :: Type -> Name -> [Type] -> Type
getResultType mbType nm ts = AppT (AppT ArrowT (ConT nm)) (AppT mbType typ)
  where
  typ = case ts of
    []  -> TupleT 0
    [t] -> t
    _   -> foldl AppT (TupleT $ length ts) ts

getCons :: Info -> (Name,[Con])
getCons = \case
  TyConI d -> case d of
    DataD _ nm _ cs _ -> (nm,cs)
    NewtypeD _ nm _ c _ -> (nm,[c])
    _ -> error "Unsupported datatype"
  _ -> error "Not a datatype"

alterName :: String -> Name -> Name
alterName prefix (Name (OccName nm) _) = Name (OccName (prefix ++ nm)) NameS

-- }}}

-- Decl builders {{{

projectMulti :: Name -> Name -> Name -> [Name] -> Q Dec
projectMulti funNm constrNm argNm fieldNms = funD' funNm $
  clause' (varP argNm) $
  normalB $ caseE (varE argNm) $
    [ match' (conP constrNm $ map varP fieldNms) $
        normalB $ appE [|Just|] $ tupE $ map varE fieldNms
    , match' wildP $ normalB [|Nothing|]
    ]

projectSingle :: Name -> Name -> [Name] -> Q Dec
projectSingle funNm constrNm fieldNms = funD' funNm $
  clause' (conP constrNm $ map varP fieldNms) $
    normalB $ appE [|Just|] $ tupE $ map varE fieldNms

clause' :: PatQ -> BodyQ -> ClauseQ
clause' x y = clause [x] y []

match' :: PatQ -> BodyQ -> MatchQ
match' x y = match x y []

funD' :: Name -> ClauseQ -> DecQ
funD' x y = funD x [y]

-- }}}

