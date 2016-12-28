{-# 
LANGUAGE 
  FlexibleInstances, 
  TemplateHaskell, 
  MultiParamTypeClasses, 
  UndecidableInstances 
#-}
module Syntax (module Unbound.LocallyNameless, 
               module Unbound.LocallyNameless.Alpha,
               n2s,
               Vnm,
               TVnm,
               Kind(..),
               Type(..),
               Term(..),
               isTerminating) where

import Unbound.LocallyNameless 
import Unbound.LocallyNameless.Alpha
      
type TVnm = Name Type

data Kind = Star
 deriving (Show,Eq)

data Type =                -- Types:
   TVar TVnm               -- Type Variables
 | Top                     -- Top type
 | Nat                     -- Natural number type
 | Unit                    -- Unit type
 | U                       -- Untyped universe
 | Arr Type Type           -- Function type
 | Prod Type Type          -- Product type
 | Forall Type (Bind TVnm Type) -- Universal quantification
 deriving Show

-- If A : Type, and isTerminating A = True, then A is a terminating
-- type (or in the syntactic class T).
isTerminating :: Type -> Bool                 
isTerminating U = False
isTerminating (Arr t1 t2) = (isTerminating t1) && (isTerminating t2)
isTerminating (Prod t1 t2) = (isTerminating t1) && (isTerminating t2)
isTerminating _ = True
 
type Vnm = Name Term            -- Variable name

data Term =
   Var Vnm                      -- Free variable
 | Triv                         -- Unit's inhabitant
 | Squash Type                  -- Injection of the retract
 | Split Type                   -- Surjection of the retract
 | Box Type                     -- Generalize to the untyped universe
 | Unbox Type                   -- Specialize the untype universe to a specific type
 | Fun Type (Bind Vnm Term)     -- \lambda-abstraction
 | TFun Type (Bind TVnm Term)   -- Type lambda-abstraction
 | App Term Term                -- Function application
 | TApp Type Term               -- Type application
 | Pair Term Term               -- Pairs
 | Fst Term                     -- First projection
 | Snd Term                     -- Second projection
 | Succ Term                    -- Successor of a natural number
 | Zero                         -- The natural number 0
 deriving Show
 
-- Derives all of the meta-machinary like substitution and
-- \alpha-equality for us.
$(derive [''Term,''Type])

instance Alpha Term
instance Alpha Type

instance Subst Term Type
instance Subst Type Type where
  isvar (TVar x) = Just (SubstName x)
  isvar _ = Nothing
instance Subst Term Term where
  isvar (Var x) = Just (SubstName x)
  isvar _ = Nothing

n2s :: Name a -> String
n2s = name2String