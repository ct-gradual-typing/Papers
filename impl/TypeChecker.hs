module TypeChecker (runTC) where


import qualified Data.Map.Strict as M    

import qualified TypeErrors as TE
import Syntax
import Pretty
    
type TyCtx = M.Map Vnm Type


-- Make a type error data type.  This will be used to throw errors
-- that can be caught and handled later.

-- Type checking type.

type TCM = TE.ReaderT TyCtx (TE.ExceptT TE.TypeError LFreshM) 
    
typeCheck :: Term -> TE.ExceptT TE.TypeError LFreshM Type
typeCheck t = TE.runReaderT (typeCheck_aux t) M.empty

runTC :: Term -> Either TE.TypeError Type
runTC t = runLFreshM $ TE.runExceptT $ typeCheck t

-- Use the Reader monad transformer with the Except monad transformer.
-- The Reader will hold onto the context.
-- 

typeCheck_aux :: Term -> TCM Type
typeCheck_aux (Var x) = undefined
{-
    case e of
      Just ty -> return ty
      Nothing -> throwError (FreeVarsError x) --why not (Var x)? It's the only Term available
 where
   e = do
        f <- ask
        M.lookup x f

typeCheck_aux ctx Triv = return.Right $ Unit
typeCheck_aux ctx Zero = return.Right $ Nat
typeCheck_aux ctx (Box ty) = return.Right $ Arr ty U
typeCheck_aux ctx (Unbox) = undefined
typeCheck_aux ctx (Succ t) = do
  r <- typeCheck_aux ctx t
  case r of
    Left m -> return.Left $ m
    Right ty ->
           case ty of
             Nat -> return.Right $ Nat
             _ -> return.Left $ "Type error (successor): "++(prettyType ty)
      
typeCheck_aux ctx (Fst t) = do
  r <- typeCheck_aux ctx t
  case r of
    Left m -> return.Left $ m
    Right ty ->
        case ty of
          Prod t1 t2 -> return.Right $ t1
          _ -> return.Left $ "Type error(first projection): "++(prettyType ty)

typeCheck_aux ctx (Snd t) = do
  r <- typeCheck_aux ctx t
  case r of
    Left m -> return.Left $ m
    Right ty ->
        case ty of
          Prod t1 t2 -> return.Right $ t2
          _ -> return.Left $ "Type error (second projection): "++(prettyType ty)

typeCheck_aux ctx (Fun ty1 b) = do
  (x, t) <- unbind b
  r <- typeCheck_aux ((x , ty1):ctx) t
  case r of
    Left m -> return.Left $ m
    Right ty2 -> return.Right $ Arr ty1 ty2

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

typeCheck_aux ctx (Pair t1 t2) = do
  r1 <- typeCheck_aux ctx t1
  r2 <- typeCheck_aux ctx t2
  return $ Prod ty1 ty2
-}