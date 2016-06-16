module TypeChecker where

import Syntax

typeCheck :: Term -> Type
typeCheck t = Nat