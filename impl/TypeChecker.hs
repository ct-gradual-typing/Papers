module TypeChecker (typeCheck) where

import Control.Monad.Reader
import Control.Monad.Except    
import qualified Data.Map.Strict as M    
    
import Syntax
import Pretty

    
--Create dictionary for TyCtx
type TyCtx = M.Map Vnm Type
--type TyCtx = [(Vnm, Type)]

-- Make a type error data type.  This will be used to throw errors
-- that can be caught and handled later.

-- Type checking type.

--type TCM = ReaderT TyCtx (ExceptT ???? LFreshM)
    
typeCheck :: Term -> Either String Type
typeCheck t = undefined -- runFreshM $ typeCheck_aux [] t
{-
-- Use the Reader monad transformer with the Except monad transformer.
-- The Reader will hold onto the context.
-- 

typeCheck_aux :: Term -> TCM Type
typeCheck_aux ctx (Var x) = 
    case e of
      Just ty -> return.Right $ ty
      Nothing -> return.Left $ "Type error: variable "++(n2s x)++" is free, but I can only typecheck closed terms."
 where
   e = M.lookup x ctx
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
  case (r1 , r2) of
    (Left m1 , Left m2) -> return.Left $ m1 ++ "\n" ++ m2
    (Left m , _) -> return.Left $ m
    (_ , Left m) -> return.Left $ m
    (Right ty1, Right ty2) -> return.Right $ Prod ty1 ty2
-}