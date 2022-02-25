{-# LANGUAGE TemplateHaskell #-}
module Metalift where

import Var
import Language.Haskell.TH
import Control.Monad
import Preprocess

lift :: [Dec] -> Q [Dec]
lift decs =
  let names = getLiftedFunctionNames decs
  in traverse (liftDec names) decs

pure' :: Exp
pure' = VarE $ mkName "pure"

bind' :: Exp
bind' = VarE $ mkName ">>="

star :: Exp
star = VarE $ mkName "<*>"

liftDec :: [Name] -> Dec -> Q Dec
liftDec ns d =
  case d of
    FunD name clauses -> liftFunD ns name clauses
    ValD pat body decs -> liftValD ns pat body decs
    SigD name typ -> liftSigD ns name typ

getName :: Exp -> Maybe Name
getName (VarE n) = Just n
getName _ = Nothing

liftExp :: [Name] -> Exp -> Q Exp
liftExp ns e =
  case e of
    VarE name ->
      if name `elem` ns
      then pure e
      else liftVal e
    LitE _ -> liftVal e
    AppE f x -> do
      case getName f of
        Nothing -> join (liftAp <$> (liftExp ns f) <*> (liftExp ns x))
        Just n | n `elem` ns -> do
          x' <- liftExp ns x
          pure $ AppE (AppE bind' x') f
        _ -> join (liftAp <$> (liftExp ns f) <*> (liftExp ns x))
    CondE c t a -> liftCond ns c t a
    CaseE c ms -> liftCase ns c ms
    LamE pats b -> liftLam ns pats b
    ConE n -> liftCon n
    LetE ds e -> liftLet ns ds e
    _ -> error $ "Unhandled construct: " <> show e

liftVal :: Exp -> Q Exp
liftVal e = pure $ AppE pure' e

liftAp :: Exp -> Exp -> Q Exp
liftAp f x = pure $ AppE (AppE star f) x

liftCase :: [Name] -> Exp -> [Match] -> Q Exp
liftCase ns e ms = do
  e' <- liftExp ns e
  ms' <- mapM (liftMatch ns) ms
  arg <- newName "e"
  let argP = VarP arg
      argE = VarE arg
  pure $ AppE (AppE bind' e') (LamE [argP] (CaseE argE ms'))

liftCond :: [Name] -> Exp -> Exp -> Exp -> Q Exp
liftCond ns c t e = do
  cn <- newName "c"
  tn <- newName "t"
  en <- newName "e"
  [| do
    $(varP cn) <- $(liftExp ns c)
    $(varP tn) <- $(liftExp ns t)
    $(varP en) <- $(liftExp ns e)
    pure $ if $(varE cn) then $(varE tn) else $(varE en)
    |]

liftLam :: [Name] -> [Pat] -> Exp -> Q Exp
-- liftLam pats e = pure $ AppE pure' $ LamE pats e -- shallow lifting
liftLam ns pats e = do -- deep lifting
  e' <- liftExp ns e
  pure $ LamE pats e'

liftMatch :: [Name] -> Match -> Q Match
liftMatch ns (Match pat body decs) = do
  body' <- liftBody ns body
  pure $ Match pat body' decs

liftBody :: [Name] -> Body -> Q Body
liftBody ns (NormalB e) = liftExp ns e >>= pure . NormalB
liftBody ns (GuardedB gs) = error "Unhandled construct: GuardedB"

liftCon :: Name -> Q Exp
liftCon n = [| pure $(conE n) |]

liftLet :: [Name] -> [Dec] -> Exp -> Q Exp
liftLet ns ds e = do
  ds' <- mapM (liftDec ns) ds
  e   <- liftExp ns e
  pure $ LetE ds' e

liftValD :: [Name] -> Pat -> Body -> [Dec] -> Q Dec
liftValD ns pat body decs = do
  body' <- liftBody ns body
  decs' <- mapM (liftDec ns) decs
  pure $ ValD pat body' decs'

liftFunD :: [Name] -> Name -> [Clause] -> Q Dec
liftFunD ns name clauses = do
  clauses' <- mapM (liftClause ns) clauses
  pure $ FunD name clauses'

liftClause :: [Name] -> Clause -> Q Clause
liftClause ns (Clause pats body decs) = do
  body' <- liftBody ns body
  decs' <- mapM (liftDec ns) decs
  pure $ Clause pats body' decs'

liftSigD :: [Name] -> Name -> Type -> Q Dec
liftSigD ns name typ =
  case typ of
    AppT (AppT ArrowT (ConT t)) (ConT t') -> pure $ SigD name typ'
      where typ' = AppT (AppT ArrowT (ConT t)) (AppT (ConT vn) (ConT t'))
            vn   = mkName "V"
    _ -> error $ "Unhandled type " <> show typ
