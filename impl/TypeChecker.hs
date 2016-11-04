module TypeChecker (runTC) where


import qualified Data.Map.Strict as M    

import qualified TypeErrors as TE
import Syntax
import Pretty
    
type TyCtx = M.Map Vnm Type
 
type TCM = TE.ReaderT TyCtx (TE.ExceptT TE.TypeError LFreshM) 


runTC :: Term -> Either TE.TypeError Type
runTC t = runLFreshM $ TE.runExceptT $ typeCheck t

    
typeCheck :: Term -> TE.ExceptT TE.TypeError LFreshM Type
typeCheck t = TE.runReaderT (typeCheck_aux t) M.empty


lookup_ctx :: Vnm -> TCM (Maybe Type)
lookup_ctx n = do
                ctx <- TE.ask
                return $ M.lookup n ctx
         

extend_ctx :: Vnm -> Type -> TCM a -> TCM a
extend_ctx x ty = TE.local (M.insert x ty)

typeCheck_aux :: Term -> TCM Type
typeCheck_aux (Var x) = do
                         maybeType <- lookup_ctx x
                         case maybeType of
                            Just found -> return $ found
                            Nothing -> TE.throwError $ TE.FreeVarsError $ x

typeCheck_aux Triv = return $ Unit
typeCheck_aux Zero = return $ Nat
typeCheck_aux (Box ty) = if(isTerminating ty)
                            then return $ Arr ty U 
                            else TE.throwError $ TE.BoxError ty

typeCheck_aux (Unbox ty) = if(isTerminating ty)
                            then return $ Arr U ty      
                            else TE.throwError $ TE.UnboxError ty

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

typeCheck_aux (App t1 t2) = do
  ty1 <- typeCheck_aux t1
  ty2 <- typeCheck_aux t2
  case ty1 of 
    Arr a b -> 
        if(a == ty2)
            then return $ b
            else TE.throwError $ TE.UnMatchedTypes ty2 a
    _ -> TE.throwError $ TE.AppError ty1 ty2
    
typeCheck_aux (Pair t1 t2) = do 
  ty1 <- typeCheck_aux t1
  ty2 <- typeCheck_aux t2
  return $ Prod ty1 ty2
  
typeCheck_aux Squash = return $ Arr (Arr U U) U

typeCheck_aux Split = return $ Arr U (Arr U U)