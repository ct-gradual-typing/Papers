{-# 
LANGUAGE 
  FlexibleInstances, 
  MultiParamTypeClasses,
  TemplateHaskell,
  UndecidableInstances,
  ViewPatterns
#-}
module TypeChecker where

import qualified Data.Map.Strict as M

import qualified TypeErrors as TE
import Syntax

data ATerm =
   ATVar Type (Name ATerm)        
 | ATTriv                         
 | ATSquash Type Type                  
 | ATSplit Type Type                   
 | ATBox Type Type                     
 | ATUnbox Type Type                   
 | ATFun  Type Type (Bind (Name ATerm) ATerm) 
 | ATTFun Type Type (Bind (Name ATerm) ATerm)
 | ATApp Type ATerm ATerm         
 | ATTApp Type Type ATerm         
 | ATPair Type ATerm ATerm        
 | ATFst Type ATerm               
 | ATSnd Type ATerm               
 | ATSucc ATerm                   
 | ATZero
 | ATNCase Type ATerm ATerm (Bind Vnm ATerm)
 | ATSub Type ATerm
 | ATEmpty Type                           
 | ATCons Type ATerm ATerm           
 | ATLCase Type ATerm ATerm (Bind Vnm (Bind Vnm ATerm))
 deriving Show

$(derive [''ATerm])

instance Alpha ATerm

getType :: ATerm -> Type
getType (ATVar ty _) = ty
getType ATTriv = Unit
getType (ATSquash ty _) = Arr ty U
getType (ATSplit ty _) = Arr U ty   
getType (ATBox ty _) = ty
getType (ATUnbox ty _) = ty
getType (ATFun ty _ _) = ty
getType (ATTFun ty _ _) = ty
getType (ATApp ty _ _) = ty
getType (ATTApp ty _ _) = ty
getType (ATPair ty _ _) = ty
getType (ATFst ty _) = ty
getType (ATSnd ty _) = ty
getType (ATSucc _) = Nat
getType ATZero = Nat
getType (ATNCase ty _ _ _) = ty
getType (ATEmpty ty) = ty
getType (ATCons ty _ _) = ty
getType (ATLCase ty _ _ _) = ty
getType (ATSub ty _) = ty

type TyCtx = M.Map Vnm Type
type TTyCtx = M.Map TVnm Type
    
type TCM = TE.ReaderT (TTyCtx, TyCtx) (TE.ExceptT TE.TypeError LFreshM) 

lookup_ctx :: Vnm -> TCM (Maybe Type)
lookup_ctx n = do
                (tctx,ctx) <- TE.ask
                return $ M.lookup n ctx         

extend_ctx :: Vnm -> Type -> TCM a -> TCM a
extend_ctx x ty = TE.local (\(tctx,ctx) -> (tctx, M.insert x ty ctx))

lookup_tctx :: TVnm -> TCM (Maybe Type)
lookup_tctx n = do
                (tctx,ctx) <- TE.ask
                return $ M.lookup n tctx

extend_tctx :: TVnm -> Type -> TCM a -> TCM a
extend_tctx x ty = TE.local (\(tctx,ctx) -> ((M.insert x ty tctx), ctx))

reqSameType :: Type -> Type -> TCM ()
reqSameType ty1 ty2 | ty1 `aeq` ty2 = return ()
                    | otherwise = TE.throwError $ TE.TypeMismatch ty1 ty2

requireArrow :: ATerm -> TCM (Type,Type)
requireArrow (getType -> Arr t1 t2) = return (t1, t2)
requireArrow (getType -> ty) = TE.throwError $ TE.NotArrowType ty

requireForall :: ATerm -> TCM (Name Type, Type, Type)
requireForall (getType -> Forall t1 b) = lunbind b $ (\(x,t2) -> return (x,t1,t2))
requireForall (getType -> ty) = TE.throwError $ TE.NotForallType ty

isSubtype :: Type -> Type -> Either TE.TypeError Bool
isSubtype ty1 ty2 = runLFreshM $ TE.runExceptT $ subtype' ty1 ty2

subtype' :: Type -> Type -> TE.ExceptT TE.TypeError LFreshM Bool
subtype' ty1 ty2 = TE.runReaderT (subtype ty1 ty2) (M.empty, M.empty)

-- ty1 <: ty2
subtype :: Type -> Type -> TCM Bool
subtype t1 t2 | t1 `aeq` t2 = return True
subtype t1 Top = return True
subtype Nat Simple = return True
subtype Unit Simple = return True
subtype (Arr s1 s2) Simple = do
  b1 <- subtype s1 Simple
  b2 <- subtype s2 Simple
  return $ b1 && b2
subtype (Prod s1 s2) Simple = do
  b1 <- subtype s1 Simple
  b2 <- subtype s2 Simple
  return $ b1 && b2
subtype (TVar x) t2 = do
  (tctx,_) <- TE.ask
  mty <- lookup_tctx x
  case mty of
    Just t3 -> t3 `subtype` t2
    Nothing -> return False
subtype (Arr s1 s2) (Arr t1 t2) = do
  b1 <- subtype t1 s1
  b2 <- subtype s2 t2
  return $ b1 && b2
subtype (Prod s1 s2) (Prod t1 t2) = do
  b1 <- subtype s1 t1
  b2 <- subtype s2 t2
  return $ b1 && b2
subtype (Forall u1 b1) (Forall u2 b2) | u1 `aeq` u2 =
    lunbind b1 $ (\(x,s2) ->
    lunbind b2 $ (\(y,t2) -> extend_tctx x u1 $ extend_tctx y u2 $ subtype s2 t2))
subtype t1 t2 = return False

type_ok :: Type -> TCM ()
type_ok (TVar x) = do
  mty <- lookup_tctx x
  case mty of
    Just ty -> type_ok ty >> return ()
    Nothing -> TE.throwError $ TE.FreeTVarsError x
type_ok (Arr t1 t2) = type_ok t1 >> type_ok t2
type_ok (Prod t1 t2) = type_ok t1 >> type_ok t2
type_ok (Forall ty b) = lunbind b $ (\(x,t) -> extend_tctx x ty $ type_ok t)                                    
type_ok _ = return ()

ctx_ok :: TCM ()
ctx_ok = do
  (tctx,ctx) <- TE.ask
  ctx_ok' (M.toList ctx) (M.toList tctx)
 where
   ctx_ok' :: [(Vnm,Type)] -> [(TVnm,Type)] -> TCM ()
   ctx_ok' ((x,ty):ctx) tctx = do
     type_ok ty
     ctx_ok' ctx tctx
   ctx_ok' _ _ = return ()

runTC :: Term -> Type -> Either TE.TypeError ATerm
runTC t ty = runLFreshM $ TE.runExceptT $ typeCheck t ty

typeCheck :: Term -> Type -> TE.ExceptT TE.TypeError LFreshM ATerm
typeCheck t ty = TE.runReaderT (typeCheck_aux t ty) (M.empty, M.empty)

typeCheck_aux :: Term -> Type -> TCM ATerm
typeCheck_aux (Pair t1 t2) ty@(Prod ty1 ty2) = do
  a1 <- typeCheck_aux t1 ty1
  a2 <- typeCheck_aux t2 ty2
  return $ ATPair ty a1 a2
typeCheck_aux t@(Pair _ _) ty = TE.throwError $ TE.NotProdType t ty
typeCheck_aux Empty ty@(List a) = return $ ATEmpty ty
typeCheck_aux Empty ty = TE.throwError $ TE.EmptyTypeError ty
typeCheck_aux (Cons h t) ty@(List a) = do
  ah <- typeCheck_aux h a
  at <- typeCheck_aux t ty
  return $ ATCons ty at at
typeCheck_aux (Cons _ _) ty = TE.throwError $ TE.ConsTypeError ty
typeCheck_aux (Fun ty b) ty'@(Arr ty1 ty2) = 
  lunbind b $ (\(x,t) -> reqSameType ty ty1 >> (extend_ctx x ty1 $ do
                   a1 <- typeCheck_aux t ty2
                   return $ ATFun ty' ty1 $ bind (translate x) a1
              ))
typeCheck_aux t@(Fun _ _) ty = TE.throwError $ TE.NotArrowTypeTerm t ty
typeCheck_aux (TFun ty b1) ty'@(Forall ty1 b2) =
    reqSameType ty ty1 >>
       (lunbind b1 $ (\(x,t) ->
        lunbind b2 $ (\(y,ty2) -> if x == y
                                  then extend_tctx x ty $ do
                                    at <- typeCheck_aux t ty2
                                    return $ ATTFun ty' ty $ bind (translate x) at
                                  else TE.throwError $ TE.TypeVariableNameMismatch x y)))
typeCheck_aux t@(TFun _ _) ty = TE.throwError $ TE.NotForallTypeTerm t ty
typeCheck_aux Zero Nat = return $ ATZero
typeCheck_aux Zero ty = TE.throwError $ TE.ZeroTypeError ty
typeCheck_aux (Succ t) Nat = typeCheck_aux t Nat >>= (return.ATSucc)
typeCheck_aux (Succ _) ty = TE.throwError $ TE.SuccTypeError ty
typeCheck_aux Triv Unit = return $ ATTriv
typeCheck_aux Triv ty = TE.throwError $ TE.TrivTypeError ty
typeCheck_aux (Split s@(Arr U U)) ty@(Arr U (Arr U U)) = return $ ATSplit (Arr U s) ty
typeCheck_aux (Split s@(Prod U U)) ty@(Arr U (Prod U U)) = return $ ATSplit (Arr U s) ty
typeCheck_aux (Split _) ty = TE.throwError $ TE.SplitTypeError ty
typeCheck_aux (Squash s@(Arr U U)) ty@(Arr (Arr U U) U) = return $ ATSquash (Arr s U) ty
typeCheck_aux (Squash s@(Prod U U)) ty@(Arr (Prod U U) U) = return $ ATSquash (Arr s U) ty
typeCheck_aux (Squash _) ty = TE.throwError $ TE.SquashTypeError ty
typeCheck_aux (Box s) ty@(Arr t U) | s `aeq` t = do
  b <- s `subtype` Simple
  if b
  then return $ ATBox (Arr s U) s
  else TE.throwError $ TE.BoxTypeError ty
typeCheck_aux (Box _) ty = TE.throwError $ TE.BoxTypeError ty
typeCheck_aux (Unbox s) ty@(Arr U t) | s `aeq` t = do
  b <- s `subtype` Simple
  if b
  then return $ ATUnbox (Arr U s) s
  else TE.throwError $ TE.UnboxTypeError ty
typeCheck_aux (Unbox _) ty = TE.throwError $ TE.UnboxTypeError ty
typeCheck_aux t ty = do
  a <- inferType t
  return $ ATSub ty a

runIR :: Term -> Either TE.TypeError Type
runIR t = r >>= return.getType
 where
   r = runLFreshM $ TE.runExceptT $ infer t

infer :: Term -> TE.ExceptT TE.TypeError LFreshM ATerm
infer t = TE.runReaderT (inferType t) (M.empty, M.empty)

inferType :: Term -> TCM ATerm

inferType (Var x) = do
  mty <- lookup_ctx x
  case mty of
    Just found -> ctx_ok >> (return $ ATVar found (translate x))
    Nothing -> TE.throwError $ TE.FreeVarsError x

inferType Triv = return ATTriv

inferType Empty = return $ ATEmpty (Forall Top (bind (s2n "X") (List (TVar (s2n "X")))))

inferType (Cons h t) = do
  ah <- inferType h
  let hty = getType ah
  at <- typeCheck_aux t (List hty)
  return $ ATCons (List hty) ah at

inferType (LCase t t1 b) =
    lunbind b $ (\(x,b') -> do
      lunbind b' $ (\(y,t2) -> do
      at <- inferType t
      let tty = getType at
      case tty of
        List ety -> do
          at1 <- inferType t1
          extend_ctx x ety $ extend_ctx y (List ety) $ do
              at2 <- inferType t2
              let ty1 = getType at1
              let ty2 = getType at2
              if ty1 `aeq` ty2
              then return $ ATLCase ty1 at at1 (bind x (bind y at2))
              else TE.throwError $ TE.LCaseBranchesMistype ty1 ty2
        _ -> TE.throwError $ TE.LCaseScrutinyTypeError t tty))

inferType (Box ty) = do
             b <- ty `subtype` Simple
             if b
             then return $ ATBox (Arr ty U) ty
             else TE.throwError $ TE.BoxError ty

inferType (Unbox ty) = do
             b <- ty `subtype` Simple
             if b
             then return $ ATUnbox (Arr U ty) ty
             else TE.throwError $ TE.BoxError ty

inferType (Split ty@(Arr U U)) = return $ ATSplit (Arr U ty) ty
inferType (Split ty@(Prod U U)) = return $ ATSplit (Prod U ty) ty
inferType (Split ty) = TE.throwError $ TE.SplitTypeError ty

inferType (Squash ty@(Arr U U)) = return $ ATSquash (Arr U ty) ty
inferType (Squash ty@(Prod U U)) = return $ ATSquash (Prod U ty) ty
inferType (Squash ty) = TE.throwError $ TE.SquashTypeError ty

inferType Zero = return ATZero
inferType (Succ t) = do
  at <- typeCheck_aux t Nat
  return $ ATSucc at

inferType (NCase t t1 b) =
    lunbind b $ (\(x,t2) -> do
      at <- typeCheck_aux t Nat
      at1 <- inferType t1
      extend_ctx x Nat $ do
        at2 <- inferType t2
        let ty1 = getType at1
        let ty2 = getType at2
        if ty1 `aeq` ty2
        then return $ ATNCase ty1 at at1 (bind x at2)
        else TE.throwError $ TE.NCaseBranchesMistype ty1 ty2)

inferType (Pair t1 t2) = do 
  at1 <- inferType t1
  at2 <- inferType t2
  return $ ATPair (Prod (getType at1) (getType at2)) at1 at2

inferType (Fst t) = do
  at <- inferType t
  case (getType at) of
   Prod t1 t2 -> return $ ATFst t1 at
   _ -> TE.throwError $ TE.FstError $ (Fst t)

inferType (Snd t) = do
  at <- inferType t
  case (getType at) of
   Prod t1 t2 -> return $ ATSnd t2 at
   _ -> TE.throwError $ TE.FstError $ (Snd t)

inferType (Fun ty1 b) = do
  lunbind b $ (\(x,t) ->
      extend_ctx x ty1 $ do
        at <- inferType t
        return $ ATFun (Arr ty1 (getType at)) ty1 $ bind (translate x) at)

inferType (TFun ty b) = do
  lunbind b $ (\(x,t) ->
      extend_tctx x ty $ do
        at <- inferType t
        return $ ATTFun (Forall ty (bind x $ (getType at))) ty $ bind (translate x) at)

inferType (App t1 t2) = do
  at1 <- inferType t1
  (ty1,ty2) <- requireArrow at1
  at2 <- typeCheck_aux t2 ty1
  return $ ATApp ty2 at1 at2

inferType (TApp ty2 t) = do
  at <- inferType t
  (x,ty1,ty) <- requireForall at
  b <- ty2 `subtype` ty1
  if b
  then (return $ ATTApp (subst (translate x) ty2 ty) ty2 at)
  else TE.throwError $ TE.SubtypeError ty2 ty1
