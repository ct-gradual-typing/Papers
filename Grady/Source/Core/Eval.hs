module Core.Eval where

import Core.Syntax
import TypeErrors

type EM = ExceptT TypeError LFreshM

eval :: CTerm -> Either TypeError CTerm
eval t = runLFreshM $ runExceptT $ evalCTerm t

evalCTerm :: CTerm -> EM CTerm
evalCTerm t = evalCTerm' t

evalCTerm' :: CTerm -> EM CTerm

evalCTerm' (CPair t1 t2) = do
  e1 <- evalCTerm' t1
  e2 <- evalCTerm' t2
  return $ CPair e1 e2

evalCTerm' (CFst t) = do
  e <- evalCTerm' t
  return $ case e of
             CPair e1 e2 -> e1
             _ -> CFst e
evalCTerm' (CSnd t) = do
  e <- evalCTerm' t
  return $ case e of 
             CPair e1 e2 -> e2
             _ -> CSnd e
evalCTerm' (CSucc t) = evalCTerm' t >>= (\e -> return $ CSucc e)

evalCTerm' (CNCase t t1 b) = lunbind b $ (\(x,t2) ->
   do e <- evalCTerm' t            
      case e of
        CZero -> evalCTerm' t1
        CSucc e' -> evalCTerm' $ subst x e' t2
        _ -> return $ CNCase e t1 (bind x t2))

evalCTerm' (CLCase t ty t1 b) =    
           do e <- evalCTerm' t
              case e of
                CEmpty -> evalCTerm' t1
                CCons e' et ->
                    lunbind b  $ (\(x,b') ->
                    lunbind b' $ (\(y,t2) -> evalCTerm' $ subst x e' $ subst y et t2))
                _ -> return $ CLCase e ty t1 b

evalCTerm' (CCons h t) = do
  e <- evalCTerm' h
  e' <- evalCTerm' t
  return $ CCons e e'

evalCTerm' (CApp t1 t2) = do
  e1 <- evalCTerm' t1
  case e1 of
    CFun ty b -> lunbind b $ (\(x,e) -> evalCTerm' $ subst x t2 e)
    CSplit ty -> do
        e2 <- evalCTerm' t2
        case e2 of
          CApp (CSquash ty') e2' -> if ty `aeq` ty'
                                    then return e2'
                                    else throwError $ SplitSquashTypeError ty ty'
          _ -> return $ CApp e1 e2
    CUnbox ty -> do
        e2 <- evalCTerm' t2
        case e2 of
          CApp (CBox ty') e2' -> do
                          if (ty `aeq` ty')
                          then return e2'
                          else throwError $ UnboxBoxTypeError ty ty'
          _ -> return $ CApp e1 e2
    CBox ty -> do
      e2 <- evalCTerm' t2
      return $ CApp e1 e2
    CSquash ty -> do
      e2 <- evalCTerm' t2
      return $ CApp e1 e2
    CVar ty -> do
      e2 <- evalCTerm' t2
      return $ CApp e1 e2
    (CApp (CSplit _) (CVar x)) -> evalCTerm' t2 >>= (\e2 -> return $ CApp e1 e2)
    _ -> return $ CApp e1 t2
evalCTerm' (CTApp ty t) = do
  e <- evalCTerm' t
  case e of
    CTFun ty' b -> lunbind b $ (\(x,e') -> evalCTerm' $ subst x ty e')
    _ -> return $ CTApp ty e
evalCTerm' t = return t