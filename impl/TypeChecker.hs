module TypeChecker (typeCheck) where

import Syntax

type TyCtx = [(Vnm, Term)]

typeCheck :: Term -> Type
typeCheck t = runFreshM $ typeCheck_aux [] t

typeCheck_aux :: Fresh m => TyCtx -> Term -> m Type
typeCheck_aux ctx t = return Nat
