module Eval where

import Syntax
import TypeChecker
import TypeErrors
import Pretty

type EM = ExceptT TypeError LFreshM

eval :: Term -> Either TypeError Term
eval t = runLFreshM $ runExceptT $ evalTerm t

evalTerm :: Term -> EM Term
evalTerm t = infer t >> evalTerm' t

evalTerm' :: Term -> EM Term

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

evalTerm' (NCase t t1 b) = lunbind b $ (\(x,t2) ->
   do e <- evalTerm' t            
      case e of
        Zero -> evalTerm' t1
        Succ e' -> evalTerm' $ subst x e' t2
        _ -> return $ NCase e t1 (bind x t2))

evalTerm' (LCase t t1 b) =    
           do e <- evalTerm' t
              case e of
                Empty -> evalTerm' t1
                Cons e' et ->
                    lunbind b  $ (\(x,b') ->
                    lunbind b' $ (\(y,t2) -> evalTerm' $ subst x e' $ subst y et t2))
                _ -> return $ LCase e t1 b

evalTerm' (Cons h t) = do
  e <- evalTerm' h
  e' <- evalTerm' t
  return $ Cons e e'

evalTerm' (App t1 t2) = do
  e1 <- evalTerm' t1
  case e1 of
    Fun ty b -> lunbind b $ (\(x,e) -> evalTerm' $ subst x t2 e)
    Split ty -> do
        e2 <- evalTerm' t2
        case e2 of
          App (Squash ty') e2' -> if ty `aeq` ty'
                                  then return e2'
                                  else throwError $ SplitSquashTypeError ty ty'
          _ -> return $ App e1 e2
    Unbox ty -> do
        e2 <- evalTerm' t2
        case e2 of
          App (Box ty') e2' -> do
                          at2 <- infer e2'
                          if (getType at2) `aeq` ty
                          then return e2'
                          else throwError $ UnboxBoxTypeError (getType at2) ty                          
          _ -> return $ App e1 e2
    Box ty -> do
      e2 <- evalTerm' t2
      return $ App e1 e2
    Squash ty -> do
      e2 <- evalTerm' t2
      return $ App e1 e2
    _ -> return $ App e1 t2
evalTerm' (TApp ty t) = do
  e <- evalTerm' t
  case e of
    TFun ty' b -> lunbind b $ (\(x,e') -> evalTerm' $ subst x ty e')
    _ -> return $ TApp ty e
evalTerm' t = return t