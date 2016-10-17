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
                            else TE.throwError $ TE.BoxError $ (Box ty)

typeCheck_aux (Unbox ty) = if(isTerminating ty)
                            then return $ Arr U ty      
                            else TE.throwError $ TE.UnboxError $ (Unbox ty)

typeCheck_aux (Succ t) = do
  r <- typeCheck_aux t
  case r of
    Nat -> return $ Nat
    _ -> TE.throwError $ TE.SuccError $ (Succ t) 
     
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
  (x, t) <- unbind b
  ty2 <- typeCheck_aux t
  return $ Arr ty1 ty2
    

typeCheck_aux (App t1 t2) = do
  ty1 <- typeCheck_aux t1
  ty2 <- typeCheck_aux t2
  let ty3 = Arr ty1 ty2
  if (ty1 == ty3)
    then return $ ty2
    else TE.throwError $ TE.AppError $ (App t1 t2)
    
typeCheck_aux (Pair t1 t2) = do 
  ty1 <- typeCheck_aux t1
  ty2 <- typeCheck_aux t2
  return $ Prod ty1 ty2
   