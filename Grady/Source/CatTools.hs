module CatTools where

import Core.Syntax
import Core.Pretty

import Prelude hiding (id)

--  t1 : a --> B
--  t2 : b --> c
-- comp t1 t2 : a --> c
comp :: Type -> CTerm -> CTerm -> LFreshM CTerm
comp a t1 t2 = do
  y <- lfresh $ s2n "y"
  return $ CFun a $ bind y $ CApp t2 (CApp t1 (CVar y))

-- id a : a --> a
id :: Type -> LFreshM CTerm
id a = do
  y <- lfresh $ s2n "y"
  return $ CFun a $ bind y $ CVar y

-- t1 : c -> a
-- t2 : b -> d
-- arrow_func t1 t2 : (a -> b) --> (c -> d)
arrow_func :: Type -> Type -> Type -> CTerm -> CTerm -> LFreshM CTerm
arrow_func a b c t1 t2 = do
  f <- lfresh $ s2n "f"
  y <- lfresh $ s2n "y"
  return $ CFun (Arr a b) $ bind f $ CFun c $ bind y $ CApp t2 (CApp (CVar f) (CApp t1 (CVar y)))

-- t1 : a -> c
-- t2 : b -> d
-- prod_func t1 t2 : (a x b) --> (c x d)
prod_func :: Type -> Type -> CTerm -> CTerm -> LFreshM CTerm
prod_func a b t1 t2 = do
  f <- lfresh $ s2n "f"
  return $ CFun (Prod a b) $ bind f $ CPair (CApp t1 (CFst (CVar f))) (CApp t2 (CSnd (CVar f)))

omega :: LFreshM CTerm
omega = let f = s2n "f"
         in return $ CFun (Arr U U) $ bind f $ CApp (CVar f) (CApp (CSquash (Arr U U)) (CVar f))

fix :: LFreshM CTerm
fix = let f = s2n "f"
          x = s2n "x"
          inner = (CFun U $ bind x $ CApp (CVar f) $ CApp (CApp (CSplit U) (CVar x)) (CVar x))
       in do o <- omega
             return $ CFun (Arr U U) $ bind f $ CApp o inner

-- f : a -> b
list_func :: Type -> Type -> CTerm -> LFreshM CTerm
list_func a b f = let r = s2n "r"
                      l = s2n "l"
                      x = s2n "x"
                      xs = s2n "xs"
                   in do
                     fx <- fix
                     return $ CApp fx $ CFun (Arr (List a) (List b)) $ bind r $ CFun (List a) $ bind l $
                            CLCase (CVar l) a (CTApp b CEmpty) $ bind x $ bind xs $ CCons (CApp f (CVar x)) (CApp (CVar r) (CVar xs))
