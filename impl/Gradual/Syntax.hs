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
               SrcPos,
               Kind(..),
               Type(..),
               Term(..),
               isTerminating,
               is_atomic) where
    
import Unbound.LocallyNameless hiding (comp)
import Unbound.LocallyNameless.Alpha
      
type SrcPos = (Int, Int, String)
type TVnm = Name Type

data Kind = Star
 deriving (Show,Eq)

data Type =                -- Types:
   TVar TVnm               -- Type Variables
 | Top                     -- Top type
 | Simple                  -- Universe of simple types
 | Skeleton                -- University of skeletons
 | Nat                     -- Natural number type
 | Unit                    -- Unit type
 | U                       -- Untyped universe
 | Arr Type Type           -- Function type
 | Prod Type Type          -- Product type
 | List Type               -- List type
 | Forall Type (Bind TVnm Type) -- Universal quantification
 deriving Show

-- If A : Type, and isTerminating A = True, then A is a terminating
-- type (or in the syntactic class T).
isTerminating :: Type -> Bool                 
isTerminating U = False
isTerminating (Arr t1 t2) = (isTerminating t1) && (isTerminating t2)
isTerminating (Prod t1 t2) = (isTerminating t1) && (isTerminating t2)
isTerminating _ = True

-- Tests to determine if a type is atomic.
is_atomic :: Type -> Bool
is_atomic (Arr _ _) = False
is_atomic (Prod _ _) = False
is_atomic _ = True

type Vnm = Name Term                          -- Variable name

data Term =
   Var Vnm                                    -- Free variable
 | Triv                                       -- Unit's inhabitant
 | Box Type SrcPos                            -- Generalize to the untyped universe
 | Unbox Type  SrcPos                         -- Specialize the untype universe to a specific type   
 | Fun Type (Bind Vnm Term)                   -- \lambda-abstraction
 | TFun Type (Bind TVnm Term)                 -- Type lambda-abstraction
 | App Term Term                              -- Function application
 | TApp Type Term                             -- Type application
 | Pair Term Term                             -- Pairs
 | Fst Term                                   -- First projection
 | Snd Term                                   -- Second projection
 | Succ Term                                  -- Successor of a natural number
 | Zero                                       -- The natural number 0
 | NCase Term Term (Bind Vnm Term)            -- Natural number eliminator
 | Empty                                      -- Empty list
 | Cons Term Term                             -- Cons
 | LCase Term Term (Bind Vnm (Bind Vnm Term)) -- List Eliminator
 deriving Show
 
-- Derives all of the meta-machinary like substitution and
-- \alpha-equality for us.
$(derive [''Term,''Type])

instance Alpha Term
instance Alpha Type

instance Subst Term Type
instance Subst Type Term where
instance Subst Type Type where
  isvar (TVar x) = Just (SubstName x)
  isvar _ = Nothing
instance Subst Term Term where
  isvar (Var x) = Just (SubstName x)
  isvar _ = Nothing

n2s :: Name a -> String
n2s = name2String