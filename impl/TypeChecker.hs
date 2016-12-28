{-# LANGUAGE ViewPatterns #-}
module TypeChecker (runTC) where


import qualified Data.Map.Strict as M    

import qualified TypeErrors as TE
import Syntax
import Pretty
    
type TyCtx = M.Map Vnm Type
type TTyCtx = M.Map TVnm Kind

lookup_ctx :: Vnm -> TCM (Maybe Type)
lookup_ctx n = do
                (tctx,ctx) <- TE.ask
                return $ M.lookup n ctx         

extend_ctx :: Vnm -> Type -> TCM a -> TCM a
extend_ctx x ty = TE.local (\(tctx,ctx) -> (tctx, M.insert x ty ctx))

lookup_tctx :: TVnm -> TCM (Maybe Kind)
lookup_tctx n = do
                (tctx,ctx) <- TE.ask
                return $ M.lookup n tctx

extend_tctx :: TVnm -> TCM a -> TCM a
extend_tctx x = TE.local (\(tctx,ctx) -> ((M.insert x Star tctx), ctx))

kindCheck :: Type -> TCM Kind
kindCheck (TVar x) = do
  maybeType <- lookup_tctx x
  case maybeType of
    Just ty -> return $ ty
    Nothing -> TE.throwError $ TE.FreeTVarsError $ x
kindCheck (Arr a b) = kindCheck a >> kindCheck b
kindCheck (Prod a b) = kindCheck a >> kindCheck b
kindCheck (Forall a) = lunbind a $ \(x,t) -> extend_tctx x $ kindCheck t
kindCheck _ = return Star

type TCM = TE.ReaderT (TTyCtx, TyCtx) (TE.ExceptT TE.TypeError LFreshM) 

runTC :: Term -> Either TE.TypeError Type
runTC t = runLFreshM $ TE.runExceptT $ typeCheck t

typeCheck :: Term -> TE.ExceptT TE.TypeError LFreshM Type
typeCheck t = TE.runReaderT (typeCheck_aux t) (M.empty, M.empty)

typeCheck_aux :: Term -> TCM Type
typeCheck_aux (Var x) = do
                         maybeType <- lookup_ctx x
                         case maybeType of
                            Just found -> return $ found
                            Nothing -> TE.throwError $ TE.FreeVarsError $ x

typeCheck_aux Triv = return $ Unit
typeCheck_aux Zero = return $ Nat
typeCheck_aux (Box ty) =
    case ty of
      TVar _ -> return $ Arr ty U
      Nat -> return $ Arr ty U
      Unit -> return $ Arr ty U
      _ -> TE.throwError $ TE.BoxError ty

typeCheck_aux (Unbox ty) =
    case ty of
      TVar _ -> return $ Arr U ty
      Nat -> return $ Arr U ty
      Unit -> return $ Arr U ty
      _ -> TE.throwError $ TE.UnboxError ty
                         
typeCheck_aux (Succ t) = do
  r <- typeCheck_aux t
  case r of
    Nat -> return $ Nat
    _ -> TE.throwError $ TE.SuccError $ t 
     
typeCheck_aux (Fst t) = do
  r <- typeCheck_aux t
  case r of
   Prod t1 t2 -> return $ t1
   _ -> TE.throwError $ TE.FstError $ (Fst t)

typeCheck_aux (Snd t) = do
  r <- typeCheck_aux t
  case r of
   Prod t1 t2 -> return $ t2
   _ -> TE.throwError $ TE.SndError $ (Snd t)
   
typeCheck_aux (Fun ty1 b) = do
  lunbind b $ (\(x,t) ->
      extend_ctx x ty1 $ do
        ty2 <- typeCheck_aux t
        return $ Arr ty1 ty2)

typeCheck_aux (TFun b) = do
  lunbind b $ (\(x,t) ->
      extend_tctx x $ do
        ty <- typeCheck_aux t
        return $ Forall (bind x $ ty))

typeCheck_aux (App t1 t2) = do
  ty1 <- typeCheck_aux t1
  ty2 <- typeCheck_aux t2
  case ty1 of 
    Arr a b -> 
        if(a `aeq` ty2)
            then return $ b
            else TE.throwError $ TE.UnMatchedTypes ty2 a
    _ -> TE.throwError $ TE.AppError ty1 ty2

typeCheck_aux (TApp ty t) = kindCheck ty >> do  
  ty1 <- typeCheck_aux t
  case ty1 of
    Forall b -> do
      lunbind b $ (\(x,ty2) -> return $ subst x ty ty2)
    _ -> TE.throwError $ TE.TAppError t ty ty1
    
typeCheck_aux (Pair t1 t2) = do 
  ty1 <- typeCheck_aux t1
  ty2 <- typeCheck_aux t2
  return $ Prod ty1 ty2
  
typeCheck_aux (Squash ty) | ty `aeq` (Arr U U) = return $ Arr (Arr U U) U
                          | ty `aeq` (Prod U U) = return $ Arr (Prod U U) U
                          | otherwise = TE.throwError $ TE.SError ty
                          
typeCheck_aux (Split ty)  | ty `aeq` (Arr U U) = return $ Arr U (Arr U U)
                          | ty `aeq` (Prod U U) = return $ Arr U (Prod U U)
                          | otherwise = TE.throwError $ TE.SError ty