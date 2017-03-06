{-# 
LANGUAGE 
  FlexibleInstances, 
  TemplateHaskell, 
  MultiParamTypeClasses, 
  UndecidableInstances 
#-}
module Surface.Syntax (module Unbound.LocallyNameless, 
               module Unbound.LocallyNameless.Alpha,
               module Names,
               module TypeSyntax,
               Vnm,
               Term(..)) where

import Unbound.LocallyNameless hiding (comp)
import Unbound.LocallyNameless.Alpha

import Names
import TypeSyntax

type Vnm = Name Term                          
data Term =
   Var Vnm                                    -- Free variable
 | Triv                                       -- Unit's inhabitant
 | Box Type                                   -- Generalize to the untyped universe
 | Unbox Type                                 -- Specialize the untype universe to a specific type   
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


$(derive [''Term])

instance Alpha Term

instance Subst Term Type
instance Subst Type Term where
instance Subst Term Term where
  isvar (Var x) = Just (SubstName x)
  isvar _ = Nothing

