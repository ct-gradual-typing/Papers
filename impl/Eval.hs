module Eval where

import Syntax
import TypeChecker
import TypeErrors

type EM = ExceptT TypeError LFreshM

eval :: Term -> Either TypeError Term
eval t = runLFreshM $ runExceptT $ evalTerm t

evalTerm :: Term -> EM Term
evalTerm t = infer t >> evalTerm' t

evalTerm' :: Term -> EM Term
evalTerm' (Fun ty b) = lunbind b $ (\(x,t) -> evalTerm' t >>= (\e -> return $ Fun ty $ bind x e))
evalTerm' (TFun ty b) = lunbind b $ (\(x,t) -> evalTerm' t >>= (\e -> return $ TFun ty $ bind x e))
evalTerm' (Pair t1 t2) = do
  e1 <- evalTerm' t1
  e2 <- evalTerm' t2
  return $ Pair e1 e2
evalTerm' (Fst t) = do
  e <- evalTerm' t
  return $ case e of
             Pair e1 e2 -> e1
             _ -> Fst e
evalTerm' (Snd t) = do
  e <- evalTerm' t
  return $ case e of
             Pair e1 e2 -> e2
             _ -> Snd e
evalTerm' (Succ t) = evalTerm' t >>= (\e -> return $ Succ e)
evalTerm' (App t1 t2) = do
  e1 <- evalTerm' t1
  e2 <- evalTerm' t2
  case e1 of
    Fun ty b -> lunbind b $ (\(x,e) -> evalTerm' $ subst x e2 e)
    Split ty -> case e2 of
                  App (Squash ty') e2' -> if ty `aeq` ty'
                                          then return e2'
                                          else throwError $ SplitSquashTypeError ty ty'
                  _ -> return $ App e1 e2
    Unbox ty -> case e2 of
                  App (Box ty') e2' -> do
                          at2 <- infer e2'
                          if (getType at2) `aeq` ty
                          then do
                            is <- ty `subtype'` ty' 
                            if is
                            then return e2'
                            else throwError $ UnboxBoxTypeError ty ty'
                          else throwError $ UnboxBoxTypeError (getType at2) ty
                  _ -> return $ App e1 e2
    _ -> return $ App e1 e2
evalTerm' (TApp ty t) = do
  e <- evalTerm' t
  case e of
    TFun ty' b -> lunbind b $ (\(x,e') -> return $ subst x ty e')
    _ -> return $ TApp ty e
evalTerm' t = return t