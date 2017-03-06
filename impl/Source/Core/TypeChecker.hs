{-# 
LANGUAGE 
  FlexibleInstances, 
  MultiParamTypeClasses,
  TemplateHaskell,
  UndecidableInstances,
  ViewPatterns
#-}
module Core.TypeChecker where

import qualified Data.Map.Strict as M

import qualified TypeErrors as TE
import Core.Syntax
import Core.Pretty
import TypeSyntax

data ACTerm =
   ATVar Type (Name ACTerm)        
 | ATTriv                         
 | ATSquash Type Type                  
 | ATSplit Type Type                   
 | ATBox Type Type                     
 | ATUnbox Type Type                   
 | ATFun  Type Type (Bind (Name ACTerm) ACTerm) 
 | ATTFun Type Type (Bind (Name ACTerm) ACTerm)
 | ATApp Type ACTerm ACTerm         
 | ATTApp Type Type ACTerm         
 | ATPair Type ACTerm ACTerm        
 | ATFst Type ACTerm               
 | ATSnd Type ACTerm               
 | ATSucc ACTerm                   
 | ATZero
 | ATNCase Type ACTerm ACTerm (Bind CVnm ACTerm)
 | ATSub Type ACTerm
 | ATEmpty Type                           
 | ATCons Type ACTerm ACTerm           
 | ATLCase Type ACTerm ACTerm (Bind CVnm (Bind CVnm ACTerm))
 deriving Show

$(derive [''ACTerm])

instance Alpha ACTerm

getType :: ACTerm -> Type
getType (ATVar ty _) = ty
getType ATTriv = Unit
getType (ATSquash ty _) = ty
getType (ATSplit ty _) = ty   
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

type TyCtx = M.Map CVnm Type
type TTyCtx = M.Map TVnm Type
    
type TCM = TE.ReaderT (TTyCtx, TyCtx) (TE.ExceptT TE.TypeError LFreshM) 

lookup_ctx :: CVnm -> TCM (Maybe Type)
lookup_ctx n = do
                (tctx,ctx) <- TE.ask
                return $ M.lookup n ctx         

extend_ctx :: CVnm -> Type -> TCM a -> TCM a
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

requireArrow :: ACTerm -> TCM (Type,Type)
requireArrow (getType -> Arr t1 t2) = return (t1, t2)
requireArrow (getType -> ty) = TE.throwError $ TE.NotArrowType ty

requireProd :: ACTerm -> TCM (Type,Type)
requireProd (getType -> Prod ty1 ty2) = return (ty1, ty2)
requireProd (getType -> ty) = TE.throwError $ TE.NotProdType ty

requireForall :: ACTerm -> TCM (Name Type, Type, Type)
requireForall (getType -> Forall t1 b) = lunbind b $ (\(x,t2) -> return (x,t1,t2))
requireForall (getType -> ty) = TE.throwError $ TE.NotForallType ty

isSubtype :: Type -> Type -> Either TE.TypeError Bool
isSubtype ty1 ty2 = runLFreshM $ TE.runExceptT $ subtype' ty1 ty2

subtype' :: Type -> Type -> TE.ExceptT TE.TypeError LFreshM Bool
subtype' ty1 ty2 = TE.runReaderT (subtype ty1 ty2) (M.empty, M.empty)

-- ty1 <: ty2
subtype :: Type -> Type -> TCM Bool
subtype t1 t2 | t1 `aeq` t2 = return True
subtype t1 U = return True
subtype Nat Simple = return True
subtype Unit Simple = return True
subtype (List s) Simple = s `subtype` Simple
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
subtype (List s) (List t) = s `subtype` t
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
   ctx_ok' :: [(CVnm,Type)] -> [(TVnm,Type)] -> TCM ()
   ctx_ok' ((x,ty):ctx) tctx = do
     type_ok ty
     ctx_ok' ctx tctx
   ctx_ok' _ _ = return ()

runTC :: CTerm -> Type -> Either TE.TypeError ACTerm
runTC t ty = runLFreshM $ TE.runExceptT $ typeCheck t ty

typeCheck :: CTerm -> Type -> TE.ExceptT TE.TypeError LFreshM ACTerm
typeCheck t ty = TE.runReaderT (typeCheck_aux t ty) (M.empty, M.empty)

typeCheck_aux :: CTerm -> Type -> TCM ACTerm
typeCheck_aux (CVar x) ty = do
  mty <- lookup_ctx x
  case mty of
    Just found -> do
             ctx_ok
             reqSameType found ty
             return $ ATVar found (translate x)
    Nothing -> TE.throwError $ TE.CoreFreeVarsError x
typeCheck_aux (CFst t) ty = do
  aty <- inferType t
  (aty1, aty2) <- requireProd aty
  if(aty1 `aeq` ty)
   then 
    return $ ATFst aty1 aty 
   else
    TE.throwError $ TE.FstTypeError ty
typeCheck_aux (CSnd t) ty = do
  aty <- inferType t
  (aty1, aty2) <- requireProd aty
  if(aty2 `aeq` ty)
   then 
    return $ ATFst aty2 aty 
   else
    TE.throwError $ TE.SndTypeError ty
typeCheck_aux (CNCase t1 t2 b) ty' = do
  let ty = List ty'
  at1 <- typeCheck_aux t1 Nat
  at2 <- typeCheck_aux t2 ty
  lunbind b $ (\(x,t) -> do
    at <- typeCheck_aux t (getType at2)
    return $ ATNCase ty at1 at2 $ bind x at)
typeCheck_aux (CPair t1 t2) ty@(Prod ty1 ty2) = do
  a1 <- typeCheck_aux t1 ty1
  a2 <- typeCheck_aux t2 ty2
  return $ ATPair ty a1 a2
typeCheck_aux t@(CPair _ _) ty = TE.throwError $ TE.CoreNotProdTypeTerm t ty
typeCheck_aux CEmpty ty@(List a) = return $ ATEmpty ty
typeCheck_aux CEmpty ty = TE.throwError $ TE.EmptyTypeError ty
typeCheck_aux (CCons h t) ty@(List a) = do
  ah <- typeCheck_aux h a
  at <- typeCheck_aux t ty
  return $ ATCons ty at at
typeCheck_aux (CCons _ _) ty = TE.throwError $ TE.ConsTypeError ty
typeCheck_aux (CFun ty b) ty'@(Arr ty1 ty2) = 
  lunbind b $ (\(x,t) -> reqSameType ty ty1 >> (extend_ctx x ty1 $ do
                   a1 <- typeCheck_aux t ty2
                   return $ ATFun ty' ty1 $ bind (translate x) a1
              ))
typeCheck_aux t@(CFun _ _) ty = TE.throwError $ TE.CoreNotArrowTypeTerm t ty
typeCheck_aux (CTFun ty b1) ty'@(Forall ty1 b2) =
    reqSameType ty ty1 >>
       (lunbind b1 $ (\(x,t) ->
        lunbind b2 $ (\(y,ty2) -> if x == y
                                  then extend_tctx x ty $ do
                                    at <- typeCheck_aux t ty2
                                    return $ ATTFun ty' ty $ bind (translate x) at
                                  else TE.throwError $ TE.TypeVariableNameMismatch x y)))
typeCheck_aux t@(CTFun _ _) ty = TE.throwError $ TE.CoreNotForallTypeTerm t ty
typeCheck_aux CZero Nat = return $ ATZero
typeCheck_aux CZero ty = TE.throwError $ TE.ZeroTypeError ty
typeCheck_aux (CSucc t) Nat = typeCheck_aux t Nat >>= (return.ATSucc)
typeCheck_aux (CSucc _) ty = TE.throwError $ TE.SuccTypeError ty
typeCheck_aux CTriv Unit = return $ ATTriv
typeCheck_aux CTriv ty = TE.throwError $ TE.TrivTypeError ty
typeCheck_aux (CSplit s@(Arr U U)) ty@(Arr U (Arr U U)) = return $ ATSplit (Arr U s) ty
typeCheck_aux (CSplit s@(Prod U U)) ty@(Arr U (Prod U U)) = return $ ATSplit (Arr U s) ty
typeCheck_aux (CSplit _) ty = TE.throwError $ TE.SplitError ty
typeCheck_aux (CSquash s@(Arr U U)) ty@(Arr (Arr U U) U) = return $ ATSquash (Arr s U) ty
typeCheck_aux (CSquash s@(Prod U U)) ty@(Arr (Prod U U) U) = return $ ATSquash (Arr s U) ty
typeCheck_aux (CSquash _) ty = TE.throwError $ TE.SquashError ty
typeCheck_aux (CApp t1 t2) ty = do
  at1 <- inferType t1
  (ty1,ty2) <- requireArrow at1
  if ty2 `aeq` ty
  then do
    at2 <- typeCheck_aux t2 ty1
    return $ ATApp ty2 at1 at2
  else
    TE.throwError $ TE.AppTypeError ty ty2
typeCheck_aux (CBox s) ty@(Arr t U) | s `aeq` t = do
  b <- s `subtype` Simple
  if b
  then return $ ATBox (Arr s U) s
  else TE.throwError $ TE.BoxTypeError ty
typeCheck_aux (CBox _) ty = TE.throwError $ TE.BoxTypeError ty
typeCheck_aux (CUnbox s) ty@(Arr U t) | s `aeq` t = do
  b <- s `subtype` Simple
  if b
  then return $ ATUnbox (Arr U s) s
  else TE.throwError $ TE.UnboxTypeError ty
typeCheck_aux (CUnbox _) ty = TE.throwError $ TE.UnboxTypeError ty
typeCheck_aux t ty = do  
  a <- inferType t
  return $ ATSub ty a

runIR :: CTerm -> (M.Map CVnm Type) -> Either TE.TypeError Type
runIR t m = r >>= return.getType
 where
   r = runLFreshM $ TE.runExceptT $ infer t m

infer :: CTerm -> (M.Map CVnm Type) -> TE.ExceptT TE.TypeError LFreshM ACTerm
infer t m = TE.runReaderT (inferType t) (M.empty, m)

inferType :: CTerm -> TCM ACTerm

inferType (CVar x) = do
  mty <- lookup_ctx x
  case mty of
    Just found -> ctx_ok >> (return $ ATVar found (translate x))
    Nothing -> TE.throwError $ TE.CoreFreeVarsError x

inferType CTriv = return ATTriv

inferType CEmpty = return $ ATEmpty (Forall U (bind (s2n "X") (List (TVar (s2n "X")))))

inferType (CCons h t) = do
  ah <- inferType h
  let hty = getType ah
  at <- typeCheck_aux t (List hty)
  return $ ATCons (List hty) ah at

inferType (CLCase t ty t1 b) = do    
      at <- typeCheck_aux t (List ty)
      case ty of
        List ety -> do
          at1 <- inferType t1
          let ty1 = getType at1
          lunbind b $ (\(x,b') -> lunbind b' $ (\(y,t2) -> 
            extend_ctx x ety $ extend_ctx y (List ety) $ do
                      at2 <- typeCheck_aux t2 ty1
                      return $ ATLCase ty1 at at1 (bind x (bind y at2))))
        _ -> TE.throwError $ TE.CoreLCaseScrutinyTypeError t ty

inferType (CBox ty) = do
             b <- ty `subtype` Simple
             if b
             then return $ ATBox (Arr ty U) ty
             else TE.throwError $ TE.BoxError ty

inferType (CUnbox ty) = do
             b <- ty `subtype` Simple
             if b
             then return $ ATUnbox (Arr U ty) ty
             else TE.throwError $ TE.UnboxError ty

inferType (CSplit ty@(Arr U U)) = return $ ATSplit (Arr U ty) ty
inferType (CSplit ty@(Prod U U)) = return $ ATSplit (Arr U ty) ty
inferType (CSplit ty) = TE.throwError $ TE.SplitError ty

inferType (CSquash ty@(Arr U U)) = return $ ATSquash (Arr ty U) ty
inferType (CSquash ty@(Prod U U)) = return $ ATSquash (Arr ty U) ty
inferType (CSquash ty) = TE.throwError $ TE.SquashError ty

inferType CZero = return ATZero
inferType (CSucc t) = do
  at <- typeCheck_aux t Nat
  return $ ATSucc at

inferType (CNCase t t1 b) =
    lunbind b $ (\(x,t2) -> do
      at <- typeCheck_aux t Nat
      at1 <- inferType t1
      let ty1 = getType at1
      extend_ctx x Nat $ do
        at2 <- typeCheck_aux t2 ty1
        return $ ATNCase ty1 at at1 (bind x at2))

inferType (CPair t1 t2) = do 
  at1 <- inferType t1
  at2 <- inferType t2
  return $ ATPair (Prod (getType at1) (getType at2)) at1 at2

inferType (CFst t) = do
  at <- inferType t
  case (getType at) of
   Prod t1 t2 -> return $ ATFst t1 at
   ty -> TE.throwError $ TE.FstTypeError ty

inferType (CSnd t) = do
  at <- inferType t
  case (getType at) of
   Prod t1 t2 -> return $ ATSnd t2 at
   ty -> TE.throwError $ TE.SndTypeError ty

inferType (CFun ty1 b) = do
  lunbind b $ (\(x,t) ->
      extend_ctx x ty1 $ do
        at <- inferType t
        return $ ATFun (Arr ty1 (getType at)) ty1 $ bind (translate x) at)

inferType (CTFun ty b) = do
  lunbind b $ (\(x,t) ->
      extend_tctx x ty $ do
        at <- inferType t
        return $ ATTFun (Forall ty (bind x $ (getType at))) ty $ bind (translate x) at)

inferType (CApp t1 t2) = do
  at1 <- inferType t1
  (ty1,ty2) <- requireArrow at1
  at2 <- typeCheck_aux t2 ty1
  return $ ATApp ty2 at1 at2

inferType (CTApp ty2 t) = do
  at <- inferType t
  (x,ty1,ty) <- requireForall at
  b <- ty2 `subtype` ty1
  if b
  then (return $ ATTApp (subst (translate x) ty2 ty) ty2 at)
  else TE.throwError $ TE.SubtypeError ty2 ty1
