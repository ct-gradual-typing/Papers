{-# LANGUAGE FlexibleInstances, TemplateHaskell, MultiParamTypeClasses, UndecidableInstances #-}
module Syntax (module Unbound.LocallyNameless, 
               module Unbound.LocallyNameless.Alpha,
               n2s,
               Vnm,
               Type(..),
               Term(..)) where

import Unbound.LocallyNameless 
import Unbound.LocallyNameless.Alpha
      
data Type =               -- Types:
   Nat                          -- Natural number type
 | Unit                         -- Unit type
 | U                            -- Untyped universe
 | Arr Type Type                -- Function type
 | Prod Type Type               -- Product type
 deriving (Show, Eq)
 
type Vnm = Name Term            -- Variable name

data Term =
   Var Vnm                      -- Free varialbe
 | Triv                         -- Unit's inhabitant
 | Sqsh							-- Injection of the retract
 | Split						-- Surjection of the retract
 | BoxT							-- Generalize to the untyped universe
 | Unbox						-- Specialize the untype universe to a specific type
 -- | Gen Type                     -- Generalize
 -- | Spec Type                    -- Specialize   
 | Fun Type (Bind Vnm Term)     -- \lambda-abstraction
 | App Term Term                -- Function application
 | Pair Term Term               -- Pairs
 | Fst Term                     -- First projection
 | Snd Term                     -- Second projection
 | Succ Term                    -- Successor of a natural number
 | Zero                         -- The natural number 0
 deriving Show
 
data TypeGen =
	Type						-- Terminating types
 |	Qmark						-- Untyped Universe
 |	TypeFunc TypeGen TypeGen	-- Function type
 deriving Show

	
	

-- Derives all of the meta-machinary like substitution and
-- \alpha-equality for us.
$(derive [''Term,''Type,TypeGen])
instance Alpha Term
instance Alpha Type
instance Alpha TypeGen

instance Subst Term Type
instance Subst Term Term where
  isvar (Var x) = Just (SubstName x)
  isvar _ = Nothing
instance Subst Term TypeGen

n2s :: Name a -> String
n2s = name2String