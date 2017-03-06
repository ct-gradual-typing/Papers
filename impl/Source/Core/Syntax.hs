{-# 
LANGUAGE 
  FlexibleInstances, 
  TemplateHaskell, 
  MultiParamTypeClasses, 
  UndecidableInstances 
#-}
module Core.Syntax (module Unbound.LocallyNameless,
                   module Unbound.LocallyNameless.Alpha,
                   module Names,
                   module TypeSyntax,
                   CVnm,
                   CTerm(..)) where
    
import Names
import TypeSyntax

import Unbound.LocallyNameless
import Unbound.LocallyNameless.Alpha

type CVnm = Name CTerm                         

data CTerm =
   CVar CVnm                                                -- Free variable
 | CTriv                                                    -- Unit's inhabitant
 | CSquash Type                                             -- Injection of the retract
 | CSplit Type                                              -- Surjection of the retract
 | CBox Type                                                -- Generalize to the untyped universe
 | CUnbox Type                                              -- Specialize the untype universe to a specific type
 | CFun Type (Bind CVnm CTerm)                              -- \lambda-abstraction
 | CTFun Type (Bind TVnm CTerm)                             -- Type lambda-abstraction
 | CApp CTerm CTerm                                         -- Function application
 | CTApp Type CTerm                                         -- Type application
 | CPair CTerm CTerm                                        -- Pairs
 | CFst CTerm                                               -- First projection
 | CSnd CTerm                                               -- Second projection
 | CSucc CTerm                                              -- Successor of a natural number
 | CZero                                                    -- The natural number 0
 | CNCase CTerm CTerm (Bind CVnm CTerm)                     -- Natural number eliminator
 | CEmpty                                                   -- Empty list
 | CCons CTerm CTerm                                        -- Cons
 | CLCase CTerm Type CTerm (Bind CVnm (Bind CVnm CTerm))    -- List Eliminator
 deriving Show
 
$(derive [''CTerm])

instance Alpha CTerm

instance Subst CTerm Type
instance Subst Type CTerm where
instance Subst CTerm CTerm where
  isvar (CVar x) = Just (SubstName x)
  isvar _ = Nothing
