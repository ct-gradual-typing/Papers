{-# LANGUAGE ViewPatterns #-}
module TypeChecker where


import qualified Data.Map.Strict as M    

import qualified TypeErrors as TE
import Syntax
    
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

-- ty1 <: ty2
subtype :: Type -> Type -> TCM ()
subtype t1 t2 | t1 `aeq` t2 = return ()
subtype t1 Top = return ()
subtype (TVar x) t2 = do
  (tctx,_) <- TE.ask
  mty <- lookup_tctx x
  case mty of
    Just t3 -> if t2 `aeq` t3
               then return ()
               else TE.throwError $ TE.SubtypeError t2 t3
    Nothing -> TE.throwError $ TE.FreeTVarsError x
subtype (Arr s1 s2) (Arr t1 t2) = subtype t1 s1 >> subtype s2 t2
subtype (Prod s1 s2) (Prod t1 t2) = subtype s1 t1 >> subtype s2 t2
subtype (Forall u1 b1) (Forall u2 b2) | u1 `aeq` u2 =
    lunbind b1 $ (\(x,s2) -> lunbind b2 $ (\(y,t2) -> extend_tctx x u1 $ extend_tctx y u2 $ subtype s2 t2))
subtype t1 t2 = TE.throwError $ TE.SubtypeError t1 t2


-- runTC :: Term -> Either TE.TypeError Type
-- runTC t = runLFreshM $ TE.runExceptT $ typeCheck t

-- typeCheck :: Term -> TE.ExceptT TE.TypeError LFreshM Type
-- typeCheck t = TE.runReaderT (typeCheck_aux t) (M.empty, M.empty)

-- typeCheck_aux :: Term -> TCM Type
-- typeCheck_aux (Var x) = do
--                          maybeType <- lookup_ctx x
--                          case maybeType of
--                             Just found -> return $ found
--                             Nothing -> TE.throwError $ TE.FreeVarsError $ x

-- typeCheck_aux Triv = return $ Unit
-- typeCheck_aux Zero = return $ Nat
-- typeCheck_aux (Box ty) =
--     case ty of
--       TVar _ -> return $ Arr ty U
--       Nat -> return $ Arr ty U
--       Unit -> return $ Arr ty U
--       _ -> TE.throwError $ TE.BoxError ty

-- typeCheck_aux (Unbox ty) =
--     case ty of
--       TVar _ -> return $ Arr U ty
--       Nat -> return $ Arr U ty
--       Unit -> return $ Arr U ty
--       _ -> TE.throwError $ TE.UnboxError ty
                         
-- typeCheck_aux (Succ t) = do
--   r <- typeCheck_aux t
--   case r of
--     Nat -> return $ Nat
--     _ -> TE.throwError $ TE.SuccError $ t 
     
-- typeCheck_aux (Fst t) = do
--   r <- typeCheck_aux t
--   case r of
--    Prod t1 t2 -> return $ t1
--    _ -> TE.throwError $ TE.FstError $ (Fst t)

-- typeCheck_aux (Snd t) = do
--   r <- typeCheck_aux t
--   case r of
--    Prod t1 t2 -> return $ t2
--    _ -> TE.throwError $ TE.SndError $ (Snd t)
   
-- typeCheck_aux (Fun ty1 b) = do
--   lunbind b $ (\(x,t) ->
--       extend_ctx x ty1 $ do
--         ty2 <- typeCheck_aux t
--         return $ Arr ty1 ty2)

-- typeCheck_aux (TFun b) = do
--   lunbind b $ (\(x,t) ->
--       extend_tctx x $ do
--         ty <- typeCheck_aux t
--         return $ Forall (bind x $ ty))

-- typeCheck_aux (App t1 t2) = do
--   ty1 <- typeCheck_aux t1
--   ty2 <- typeCheck_aux t2
--   case ty1 of 
--     Arr a b -> 
--         if(a `aeq` ty2)
--             then return $ b
--             else TE.throwError $ TE.UnMatchedTypes ty2 a
--     _ -> TE.throwError $ TE.AppError ty1 ty2

-- typeCheck_aux (TApp ty t) = kindCheck ty >> do  
--   ty1 <- typeCheck_aux t
--   case ty1 of
--     Forall b -> do
--       lunbind b $ (\(x,ty2) -> return $ subst x ty ty2)
--     _ -> TE.throwError $ TE.TAppError t ty ty1
    
-- typeCheck_aux (Pair t1 t2) = do 
--   ty1 <- typeCheck_aux t1
--   ty2 <- typeCheck_aux t2
--   return $ Prod ty1 ty2
  
-- typeCheck_aux (Squash ty) | ty `aeq` (Arr U U) = return $ Arr (Arr U U) U
--                           | ty `aeq` (Prod U U) = return $ Arr (Prod U U) U
--                           | otherwise = TE.throwError $ TE.SError ty
                          
-- typeCheck_aux (Split ty)  | ty `aeq` (Arr U U) = return $ Arr U (Arr U U)
--                           | ty `aeq` (Prod U U) = return $ Arr U (Prod U U)
--                           | otherwise = TE.throwError $ TE.SError ty