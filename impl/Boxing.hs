module Boxing where

import Syntax
import Parser
import Pretty

is_atomic :: Type -> Bool
is_atomic (Arr _ _) = False
is_atomic (Prod _ _) = False
is_atomic _ = True

skeleton_of :: Type -> Type
skeleton_of b | is_atomic b = U
skeleton_of (Arr a b)  = Arr (skeleton_of a) (skeleton_of b)
skeleton_of (Prod a b) = Prod (skeleton_of a) (skeleton_of b)

-- unboxing s t ty :
--   Converts t of type s into a term of type ty

unboxing :: Type -> Term -> LFreshM Term
unboxing ty t = unboxing' (skeleton_of ty) t ty

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

-- boxing ty t s :
--   Converts t of type ty into a term of type s
boxing :: Type -> Term -> LFreshM Term
boxing ty t = boxing' ty t (skeleton_of ty)

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
boxing' (Prod U U) t (Prod U U) = return t
boxing' (Prod a U) t (Prod U U) = return $ Pair (App (Box a) (Fst t)) (Snd t)
boxing' (Prod U b) t (Prod U U) = return $ Pair (Fst t) (App (Box b) (Snd t))
boxing' (Prod a b) t (Prod U U) = return $ Pair (App (Box a) (Fst t)) (App (Box b) (Snd t))
boxing' (Prod a U) t (Prod s1 U) = do
  e <- boxing' a (Fst t) s1
  return $ Pair e (Snd t)
boxing' (Prod U b) t (Prod U s2) = do
  e <- boxing' b (Snd t) s2
  return $ Pair (Fst t) e
boxing' (Prod a b) t (Prod s1 s2) = undefined
boxing' U t U = return t
boxing' ty t U = return $ App (Box ty) t
boxing' ty t s = error $ (prettyType s)++" is not a skeleton of "++(prettyType ty)++" when trying to box "++(runPrettyTerm t)

test_boxing :: String -> String -> String
test_boxing sty str = case (q,r) of
                        (Right ty, Right t) -> let b = runLFreshM $ boxing ty t
                                                in runLFreshM $ prettyTerm b
                        (Left m, Right _) -> m
                        (Right _, Left m) -> m
                        (Left m1, Left m2) -> m1 ++ "\n" ++ m2
 where
   r = parseTerm str
   q = parseType sty

test_unboxing :: String -> String -> String
test_unboxing sty str = case (q,r) of
                        (Right ty, Right t) -> let b = runLFreshM $ unboxing ty t
                                                in runLFreshM $ prettyTerm b
                        (Left m, Right _) -> m
                        (Right _, Left m) -> m
                        (Left m1, Left m2) -> m1 ++ "\n" ++ m2
 where
   r = parseTerm str
   q = parseType sty

test_boxing_unboxing :: String -> String -> String
test_boxing_unboxing sty str = case (q,r) of
                        (Right ty, Right t) -> let b = runLFreshM $ boxing ty t
                                                   b' = runLFreshM $ unboxing ty b
                                                in runLFreshM $ prettyTerm b'
                        (Left m, Right _) -> m
                        (Right _, Left m) -> m
                        (Left m1, Left m2) -> m1 ++ "\n" ++ m2
 where
   r = parseTerm str
   q = parseType sty


{-
  "\\(y:?).((box<Nat> (fst ((\\(x:Nat).(x, x)) (unbox<Nat> y)))), (box<Nat> (snd ((\\(x:Nat).(x, x)) (unbox<Nat> y)))))"
-} 