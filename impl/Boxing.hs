module Boxing where

import Syntax
import Parser
import Pretty

is_atomic :: Type -> Bool
is_atomic (Arr _ _) = False
is_atomic (Prod _ _) = False
is_atomic _ = True

is_skeleton_of :: Type -> Type -> Bool
is_skeleton_of U b | is_atomic b = True
is_skeleton_of (Arr a b) (Arr c d) = (is_skeleton_of a c) && (is_skeleton_of b d)
is_skeleton_of (Prod a b) (Prod c d) = (is_skeleton_of a c) && (is_skeleton_of b d)
is_skeleton_of _ _ = False

free_vars :: Term -> [Vnm]
free_vars t = (fv t) :: [Vnm]
                     
avoid_fv :: LFresh m => [Vnm] -> m a -> m a
avoid_fv l = avoid $ map AnyName l
                     
-- boxing s x ty :
--   Converts x of type s into a term of type ty
-- unboxing :: Type -> Vnm -> Type -> Maybe (LFreshM Term)
-- unboxing s x ty | is_skeleton_of s ty = Just $ unboxing' s x ty
--                   | otherwise = Nothing

unboxing' :: Type -> Term -> Type -> LFreshM Term
unboxing' (Arr U U) t (Arr U U) = return t
           
unboxing' (Arr U U) t (Arr U b) = do
  y <- lfresh $ s2n "y"
  return $ Fun U $ bind y $ App (Unbox b) (App t (Var y))

unboxing' (Arr U U) t (Arr a U) = do
  y <- lfresh $ s2n "y"
  return $ Fun a $ bind y $ App t (App (Box a) (Var y))

unboxing' (Arr U U) t (Arr a b) = do
  y <- lfresh $ s2n "y"
  return $ Fun a $ bind y $ App (Unbox b) (App t (App (Box a) (Var y)))

unboxing' (Arr s U) t (Arr a U) = do
  y <- lfresh $ s2n "y"
  e <- boxing' a (Var y) s
  return $ Fun a $ bind y $ App t e

unboxing' (Arr s U) t (Arr a b) = do
  y <- lfresh $ s2n "y"
  e <- boxing' a (Var y) s
  return $ Fun a $ bind y $ App (Unbox b) (App t e)

unboxing' (Arr U s) t (Arr U b) = do
  y <- lfresh $ s2n "y"
  e <- unboxing' s (App t (Var y)) b
  return $ Fun U $ bind y $ e

unboxing' (Arr U s) t (Arr a b) = do  
  y <- lfresh $ s2n "y"
  e <- unboxing' s (App t (App (Box a) (Var y))) b
  return $ Fun a $ bind y $ e

unboxing' (Arr s1 s2) t (Arr a b) = do
  y <- lfresh $ s2n "y"
  e <- boxing' a (Var y) s1
  e' <- unboxing' s2 (App t e) b        
  return $ Fun a $ bind y $ e'

unboxing' (Prod U U) t (Prod U U) = return t

unboxing' (Prod U U) t (Prod U b) =
    return $ Pair (Fst t)
                  (App (Unbox b) (Snd t))

unboxing' (Prod U U) t (Prod a U) =
    return $ Pair (App (Unbox a) (Fst t))
                  (Snd t)

unboxing' (Prod U U) t (Prod a b) =
    return $ Pair (App (Unbox a) (Fst t))
                     (App (Unbox b) (Snd t))

unboxing' (Prod s U) t (Prod a b) = do
  e <- unboxing' s (Fst t) a
  return $ Pair e (App (Unbox b) (Snd t))

unboxing' (Prod U s) t (Prod a b) = do
  e <- unboxing' s (Snd t) b
  return $ Pair (App (Unbox a) (Fst t)) e

unboxing' (Prod s1 s2) t (Prod a b) = do
  e1 <- unboxing' s1 (Fst t) a
  e2 <- unboxing' s2 (Snd t) b
  return $ Pair e1 e2

unboxing' U t U = return $ t
unboxing' U t ty = return $ App (Unbox ty) t
unboxing' s t ty = error $ (prettyType s)++" is not a skeleton of "++(prettyType ty)++" when trying to unbox "++(runPrettyTerm t)

-- boxing ty x s :
--   Converts x of type ty into a term of type s
boxing' :: Type -> Term -> Type -> LFreshM Term
boxing' (Arr U U) t (Arr U U) = return t
boxing' (Arr a U) t (Arr U U) = do
  y <- lfresh $ s2n "y"
  return $ Fun U $ bind y $ App t (App (Unbox a) (Var y))
boxing' (Arr U b) t (Arr U U) = do
  y <- lfresh $ s2n "y"
  return $ Fun U $ bind y $ App (Box b) (App t (Var y))
boxing' (Arr a b) t (Arr U U) = do
  y <- lfresh $ s2n "y"
  return $ Fun U $ bind y $ App (Box b) (App t (App (Unbox a) (Var y)))
boxing' (Arr a U) t (Arr s1 U) = do
  y <- lfresh $ s2n "y"
  e <- unboxing' s1 (Var y) a
  return $ Fun a $ bind y $ App t e
boxing' (Arr U b) t (Arr U s2) = do
  y <- lfresh $ s2n "y"
  e <- boxing' b (App t (Var y)) s2
  return $ Fun U $ bind y e
boxing' (Arr a b) t (Arr s1 s2) = do
  y <- lfresh $ s2n "y"
  e1 <- unboxing' s1 (Var y) a
  e2 <- boxing' b (App t e1) s2        
  return $ Fun s1 $ bind y $ e2
boxing' ty t s = undefined