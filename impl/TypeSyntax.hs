{-# 
LANGUAGE 
  FlexibleInstances, 
  TemplateHaskell, 
  MultiParamTypeClasses, 
  UndecidableInstances 
#-}
module TypeSyntax (module Unbound.LocallyNameless, 
                   module Unbound.LocallyNameless.Alpha,
                   module Names,
                   TVnm,
                   Kind(..),
                   Type(..),
                   is_atomic) where

import Names

import Unbound.LocallyNameless
import Unbound.LocallyNameless.Alpha

type TVnm = Name Type

data Kind = Star
 deriving (Show,Eq)

data Type =                -- Types:
   TVar TVnm               -- Type Variables
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

$(derive [''Type]) 
instance Alpha Type

instance Subst Type Type where
  isvar (TVar x) = Just (SubstName x)
  isvar _ = Nothing
    
is_atomic :: Type -> Bool
is_atomic (Arr _ _) = False
is_atomic (Prod _ _) = False
is_atomic _ = True