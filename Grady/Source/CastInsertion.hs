module CastInsertion where

import qualified Data.Map.Strict as M

import Surface.Syntax
import Core.Syntax
import Surface.TypeChecker
import CatTools as CT
import Skeleton
import TypeErrors

toTCM :: LFreshM a -> TCM a
toTCM = return.runLFreshM

insertCasts :: Term -> Either TypeError (CTerm, Type)
insertCasts t = runLFreshM $ runExceptT $ runReaderT (insert_casts t) (M.empty, M.empty)

caster :: Type -> Type -> TCM CTerm
caster ty1 ty2 | ty1 `aeq` ty2 = do
  toTCM $ CT.id ty1
caster ty U = toTCM $ box ty
caster U ty = toTCM $ unbox ty
caster (List ty1) (List ty2) = do
  c <- caster ty1 ty2
  toTCM $ CT.list_func ty1 ty2 c
caster (Prod ty1 ty2) (Prod sy1 sy2) = do
  c1 <- caster ty1 sy1
  c2 <- caster ty2 sy2
  toTCM $ CT.prod_func ty1 sy1 c1 c2
caster (Arr ty1 ty2) (Arr sy1 sy2) = do
  c1 <- caster sy1 ty1
  c2 <- caster ty2 sy2
  toTCM $ CT.arrow_func ty1 ty2 sy1 c1 c2
caster (Forall ty1 b1) (Forall sy1 b2) | ty1 `aeq` sy1 = do
  lunbind b1 $ (\(x,ty2) ->
   lunbind b2 $ (\(y,sy2) ->
     extend_tctx x ty1 $ extend_tctx y ty1 $ caster ty2 sy2))
caster ty sy = throwError $ CastInsertionError ty sy

insert_casts :: Term -> TCM (CTerm, Type)
insert_casts t@(Var x) = do
  at <- inferType t
  return (CVar (translate x), getType at) 
insert_casts t@(Box ty) = do
  at <- inferType t
  return (CBox ty, getType at)
insert_casts t@(Unbox ty) = do
  at <- inferType t
  return (CUnbox ty, getType at)
insert_casts Zero = return (CZero, Nat)
insert_casts Triv = return (CTriv, Unit)
insert_casts t@(Succ t1) = do
  (t2, ty) <- insert_casts t1
  case ty of
    U -> return (CSucc (CApp (CUnbox Nat) t2), ty)
    Nat -> return (CSucc t2, ty)
    _ -> throwError $ NotNatType ty
insert_casts (NCase t t1 b) = do
  (t', ty) <- insert_casts t
  case ty of
    U -> do (t'1, a) <- insert_casts t1
            lunbind b $ (\(x,t2) -> extend_ctx x Nat $
                do (t'2, a') <- insert_casts t2
                   if a `aeq` a'
                   then return (CNCase (CApp (CUnbox Nat) t') t'1 (bind (translate x) t'2), a)
                   else throwError $ NCaseBranchesMistype a a')
    Nat -> do (t'1, a) <- insert_casts t1
              lunbind b $ (\(x,t2) -> extend_ctx x Nat $
                do (t'2, a') <- insert_casts t2
                   if a `aeq` a'
                   then return (CNCase t' t'1 (bind (translate x) t'2), a)
                   else throwError $ NCaseBranchesMistype a a')
    _ -> throwError $ LCaseScrutinyTypeError t ty
insert_casts (Pair t1 t2) = do
  (t'1, a) <- insert_casts t1
  (t'2, b) <- insert_casts t2
  return (CPair t'1 t'2, Prod a b)
insert_casts (Fst t) = do
  (t', ty) <- insert_casts t
  case ty of
    U -> return (CFst (CApp (CSplit (Prod U U)) t'), U) 
    Prod a b -> return (CFst t', a)
    _ -> throwError $ FstTypeError ty
insert_casts (Snd t) = do
  (t', ty) <- insert_casts t
  case ty of
    U -> return (CSnd (CApp (CSplit (Prod U U)) t'), U) 
    Prod a b -> return (CSnd t', b)
    _ -> throwError $ SndTypeError ty
insert_casts Empty = do
  at <- inferType Empty
  return (CEmpty, getType at)
insert_casts (Cons t1 Empty) = do
  (t'1, ty1) <- insert_casts t1                
  return (CCons t'1 CEmpty, List ty1)  
insert_casts (Cons t1 t2) = do
  (t'1, ty1) <- insert_casts t1
  (t'2, ty2) <- insert_casts t2
  case ty2 of
    List ty'1 -> do b <- ty1 `consistent` ty'1
                    c <- caster ty1 ty'1
                    if b
                    then return (CCons (CApp c t'1) t'2, List ty'1)
                    else throwError $ ListElemTypeMismatch t1 ty1
    _ -> throwError $ NotListType ty2
insert_casts (LCase t t1 b) = do
  (t', ty) <- insert_casts t
  case ty of
    U -> do (t'1, a) <- insert_casts t1
            lunbind b $ (\(x,b') -> lunbind b' $ (\(y,t2) ->
              extend_ctx x U $ extend_ctx y (List U) $
                  do (t'2, a') <- insert_casts t2
                     if a `aeq` a'
                     then return (CLCase (CApp (CSplit U) t') (List U) t'1 (bind (translate x) (bind (translate y) t'2)), a)
                     else throwError $ LCaseBranchesMistype a a'))
    List a -> do (t'1, a') <- insert_casts t1
                 lunbind b $ (\(x,b') -> lunbind b' $ (\(y,t2) ->
                   extend_ctx x a $ extend_ctx y (List a) $
                    do (t'2, a'') <- insert_casts t2
                       if a' `aeq` a''
                       then return (CLCase t' (List a) t'1 (bind (translate x) (bind (translate y) t'2)), a')
                       else throwError $ LCaseBranchesMistype a a'))
    _ -> throwError $ LCaseScrutinyTypeError t ty
insert_casts (Fun ty b) = do
  lunbind b $ (\(x,t) -> extend_ctx x ty $
      do (t', ty') <- insert_casts t
         return (CFun ty (bind (translate x) t'), Arr ty ty'))
insert_casts (App t1 t2) = do
  (t'1, ty1) <- insert_casts t1
  case ty1 of
    U -> do (t'2, ty2) <- insert_casts t2
            c <- caster ty2 U
            return (CApp (CApp (CSplit (Arr U U)) t'1) (CApp c t'2), U)
    Arr a1 b -> do (t'2, ty2) <- insert_casts t2                   
                   l <- ty2 `consistent` a1
                   if l
                   then do c <- caster ty2 a1
                           return (CApp t'1 (CApp c t'2), b)
                   else throwError $ InconsistentTypes ty2 a1
    _ -> throwError $ NonFunctionType t1 ty1
insert_casts (TFun ty b) = 
  lunbind b $ (\(x,t) -> extend_tctx x ty $
    do (t',ty') <- insert_casts t
       return (CTFun ty (bind x t'), Forall ty (bind x ty')))
insert_casts (TApp ty t) = do
  (t', ty') <- insert_casts t
  case ty' of
    Forall ty'' b -> lunbind b $ (\(x,ty''') ->
        do b <- ty `subtype` ty''
           if b
           then return (CTApp ty t', subst x ty ty''')
           else throwError $ SubtypeError ty ty'')
    _ -> throwError $ NotForallType ty'
