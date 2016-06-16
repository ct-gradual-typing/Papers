{-# LANGUAGE FlexibleInstances, TemplateHaskell, MultiParamTypeClasses, UndecidableInstances #-}
module Syntax (module Unbound.LocallyNameless, 
               module Unbound.LocallyNameless.Alpha,
               Vnm,
               Type(Nat, Unit, Arr, Prod, U),
               Term(Var, Fun, Triv, App, Pair, Fst, Snd, Zero, Succ)) where

import Unbound.LocallyNameless 
import Unbound.LocallyNameless.Alpha
      
data Type =                     -- Types:
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
 | Fun Type (Bind Vnm Term)     -- \lambda-abstraction
 | App Term Term                -- Function application
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
instance Subst Term Term where
  isvar (Var x) = Just (SubstName x)
  isvar _ = Nothing
