module CatTools where

import Syntax
import Parser
import Pretty

import Prelude hiding (id)

--  t1 : a --> B
--  t2 : b --> c
-- comp t1 t2 : a --> c
comp :: Type -> Term -> Term -> LFreshM Term
comp a t1 t2 = do
  y <- lfresh $ s2n "y"
  return $ Fun a $ bind y $ App t2 (App t1 (Var y))

-- id a : a --> a
id :: Type -> LFreshM Term
id a = do
  y <- lfresh $ s2n "y"
  return $ Fun a $ bind y $ Var y

-- t1 : c -> a
-- t2 : b -> d
-- arrow_func t1 t2 : (a -> b) --> (c -> d)
arrow_func :: Type -> Type -> Type -> Term -> Term -> LFreshM Term
arrow_func a b c t1 t2 = do
  f <- lfresh $ s2n "f"
  y <- lfresh $ s2n "y"
  return $ Fun (Arr a b) $ bind f $ Fun c $ bind y $ App t2 (App (Var f) (App t1 (Var y)))

-- t1 : a -> c
-- t2 : b -> d
-- prod_func t1 t2 : (a x b) --> (c x d)
prod_func :: Type -> Type -> Term -> Term -> LFreshM Term
prod_func a b t1 t2 = do
  f <- lfresh $ s2n "f"
  return $ Fun (Prod a b) $ bind f $ Pair (App t1 (Fst (Var f))) (App t2 (Snd (Var f)))

