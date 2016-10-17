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
typeCheck_aux (Box ty) = return $ Arr ty U
-- if ty is a terminating type then you can unbox it
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
   
{-
typeCheck_aux (Fun ty1 b) = do
  (x, t) <- unbind b
  r <- typeCheck_aux t
  case r of
    Just ty2 -> return $ Arr ty1 ty2
    Nothing -> TE.throwError $ TE.FunError $ (Fun ty1 b)
    

typeCheck_aux ctx (App t1 t2) = do
  r1 <- typeCheck_aux ctx t1
  r2 <- typeCheck_aux ctx t2
  case (r1 , r2) of
    (Left m1 , Left m2) -> return.Left $ m1 ++ "\n" ++ m2
    (Left m , _) -> return.Left $ m
    (_ , Left m) -> return.Left $ m
    (Right r3, Right ty3) ->
        case r3 of 
          Arr ty1 ty2 ->
              if (ty1 == ty3)
              then return.Right $ ty2
              else return.Left $ "Type error: types don't match "++(prettyType ty1)++" !~ "++(prettyType ty3)
          _ -> return.Left $ "Type error (application): "++(prettyType r3)

typeCheck_aux (Pair t1 t2) = do
  r1 <- typeCheck_aux t1
  r2 <- typeCheck_aux t2
  case (r1, r2) of
    (Just ty1, Just ty2) -> return $ Prod ty1 ty2
    (_,Just ty2) -> undefined
    (Just ty1, _) -> undefined
    (_,_)  -> undefined
-}