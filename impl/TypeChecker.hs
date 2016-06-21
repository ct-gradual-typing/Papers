module TypeChecker (typeCheck) where

import Syntax
import Pretty

convTy :: Type -> Type
convTy (Arr x y) =
    case (x', y') of
      (U , U) -> U
      _ -> Arr x' y'
 where
    x' = convTy x
    y' = convTy y
convTy (Prod x y) = Prod (convTy x) (convTy y)
convTy x = x

consistent :: Type -> Type -> Bool
consistent x y = consistent' (convTy x) (convTy y)
 where
   consistent' :: Type -> Type -> Bool
   consistent' Nat Nat = True
   consistent' U _ = True
   consistent' _ U = True
   consistent' (Prod x y) (Prod x' y') =
       (consistent' x x') && (consistent' y y')
   consistent' (Arr x y) (Arr x' y') =
       (consistent' x x') && (consistent' y y')
   consistent' _ _ = False
                    
type TyCtx = [(Vnm, Type)]

typeCheck :: Term -> Either String Type
typeCheck t = r >>= return.convTy
    where
      r = runFreshM $ typeCheck_aux [] t

typeCheck_aux :: Fresh m => TyCtx -> Term -> m (Either String Type)
typeCheck_aux ctx (Var x) = 
    case e of
      Just ty -> return.Right $ ty
      Nothing -> return.Left $ "Type error: variable "++(n2s x)++" is free, but I can only typecheck closed terms."
 where
   e = lookup x ctx
typeCheck_aux ctx Triv = return.Right $ Unit
typeCheck_aux ctx Zero = return.Right $ Nat
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
              if (consistent ty1 ty3)
              then return.Right $ ty2
              else return.Left $ "Type error: inconsistent type application: "++(prettyType ty1)++" !~ "++(prettyType ty3)
          _ -> return.Left $ "Type error (application): "++(prettyType r3)

typeCheck_aux ctx (Pair t1 t2) = do
  r1 <- typeCheck_aux ctx t1
  r2 <- typeCheck_aux ctx t2
  case (r1 , r2) of
    (Left m1 , Left m2) -> return.Left $ m1 ++ "\n" ++ m2
    (Left m , _) -> return.Left $ m
    (_ , Left m) -> return.Left $ m
    (Right ty1, Right ty2) -> return.Right $ Prod ty1 ty2