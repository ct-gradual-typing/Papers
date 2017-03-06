module TypeErrors (module Control.Monad.Reader,
                   module Control.Monad.Except,
                   module Control.Applicative, TypeError(..), readTypeError)where

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.Except  

import Surface.Syntax
import Core.Syntax  
import Surface.Pretty
import Core.Pretty

--Error Types
data TypeError = FreeVarsError Vnm
               | CoreFreeVarsError CVnm
               | FreeTVarsError TVnm
               | SuccError Term
               | FstError Term
               | FstTypeError Type
               | SndTypeError Type
               | SndError Term
               | FunError Term
               | AppError Type Type
               | TAppError Term Type Type
               | NoTypeError Term
               | UnMatchedTypes Type Type
               | SError Type
               | SubtypeError Type Type
               | TypeMismatch Type Type
               | NotProdTypeTerm Term Type
               | CoreNotProdTypeTerm CTerm Type                 
               | NotArrowTypeTerm Term Type
               | CoreNotArrowTypeTerm CTerm Type                 
               | NotArrowType Type
               | NotForallType Type
               | SplitSquashTypeError Type Type
               | UnboxBoxTypeError Type Type
               | NotNatType Type                 
               | NotListType Type
               | NotProdType Type                                  
               | NotForallTypeTerm Term Type
               | CoreNotForallTypeTerm CTerm Type                 
               | TypeVariableNameMismatch TVnm TVnm
               | NCaseBranchesMistype Type Type
               | AppTypeError Type Type                 
               | LCaseBranchesMistype Type Type
               | LCaseScrutinyTypeError Term Type
               | CoreLCaseScrutinyTypeError CTerm Type
               | NoError
               | CtxNotOk
               | TrivTypeError Type
               | ConsTypeError Type
               | EmptyTypeError Type
               | ZeroTypeError Type
               | SuccTypeError Type
               | CastError Type Type
               | BoxTypeError Type 
               | UnboxTypeError Type
               | UnboxError Type
               | BoxError Type
               | SquashError Type
               | SplitError Type
               | CastInsertionError Type Type
               | ListElemTypeMismatch Term Type
               | InconsistentTypes Type Type
               | NonFunctionType Term Type
  deriving(Show)

instance Monoid TypeError where
  mempty = NoError
  mappend _ r = r

readTypeError :: TypeError -> String
readTypeError (FreeVarsError a) =
    "Type error: variable " ++(n2s a) ++ " is undefined."
readTypeError (CoreFreeVarsError a) =
    "Type error: variable " ++(n2s a) ++ " is undefined."
readTypeError (FreeTVarsError a) = 
    "Type error: the type " ++(n2s a) ++ " undefined."
readTypeError (SuccError a) = "Type error: no successor of "++runPrettyTerm a
readTypeError (FstError a) = "Type error: This is not a product type: "++runPrettyTerm a
readTypeError (FstTypeError a) = "Type error: This is not a product type: "++runPrettyType a
readTypeError (SndTypeError a) = "Type error: This is not a product type: "++runPrettyType a
readTypeError (SndError a) = "Type error: This is not a product type: "++runPrettyTerm a
readTypeError (FunError a) = "Type error (function): "++runPrettyTerm a
readTypeError (AppError a b) = "Type error (application): types don't match " ++runPrettyType a++" !~ "++(runPrettyType b)
readTypeError (TAppError a b c) = "Type error: in the term: "++runPrettyTerm a++", type " ++runPrettyType b++" is given but type "++(runPrettyType c)++" is found"
readTypeError (NoTypeError a) = "Type error: No type (" ++runPrettyTerm a++ ") was found"  
readTypeError (UnMatchedTypes a b) = "Type error: "++ (runPrettyType a) ++" must have the correct type with "++ (runPrettyType b)
readTypeError (SError a) = "Type error: "++(runPrettyType a)++" must be of type ? -> ? or (? x ?)"
readTypeError (SubtypeError a b) = "Type error: "++runPrettyType a++" is not a subtype of "++runPrettyType b
readTypeError (TypeMismatch a b) = "Type error: "++runPrettyType a++" is a mismatched type with "++runPrettyType b
readTypeError (NotProdTypeTerm t ty) = "Type error: "++runPrettyTerm t++ " should have a product type, but instead has the type "++runPrettyType ty
readTypeError (CoreNotProdTypeTerm t ty) = "Type error: "++runPrettyCTerm t++ " should have a product type, but instead has the type "++runPrettyType ty
readTypeError (NotArrowTypeTerm t ty) = "Type error: "++runPrettyTerm t++ " should been a function type but instead is of type "++runPrettyType ty
readTypeError (CoreNotArrowTypeTerm t ty) = "Type error: "++runPrettyCTerm t++ " should been a function type but instead is of type "++runPrettyType ty
readTypeError (NotArrowType a) = "Type error: "++runPrettyType a++ " is not a function type but needs to be."
readTypeError (NotForallType a) = "Type error: "++runPrettyType a++ " is not a for all type but needs to be."
readTypeError (SplitSquashTypeError a b) = "Type error: the return type is incorrect when splitting "++runPrettyType a++ " and squashing the type "++runPrettyType b 
readTypeError (UnboxBoxTypeError a b) = "Type error: the return type of unboxing "++runPrettyType a++ " does not match the input when boxing "++runPrettyType b 
readTypeError (NotNatType a) = "Type error: "++runPrettyType a++ " is not of type Nat but needs to be."
readTypeError (NotListType a) = "Type error: "++runPrettyType a++ " is not of type List but needs to be."
readTypeError (NotProdType a) = "Type error: "++runPrettyType a++ " is not of type Product but needs to be."
readTypeError (NotForallTypeTerm t ty) = "Type error: the term "++runPrettyTerm t++ " is not of type "++runPrettyType ty++" but needs to be."
readTypeError (CoreNotForallTypeTerm t ty) = "Type error: the term "++runPrettyCTerm t++ " is not of type "++runPrettyType ty++" but needs to be."
readTypeError (TypeVariableNameMismatch a b) = "Type error: "++n2s a++ " needs to match "++n2s b
readTypeError (NCaseBranchesMistype a b) = "Type error: Case error, "++runPrettyType a++ " does not match "++runPrettyType b
readTypeError (LCaseBranchesMistype a b) = "Type error: Case error, "++runPrettyType a++ " does not match "++runPrettyType b
readTypeError (LCaseScrutinyTypeError t ty) = "Type error: The scrutiny: "++runPrettyTerm t++ " cannot be cast to type: "++runPrettyType ty
readTypeError (CoreLCaseScrutinyTypeError t ty) = "Type error: The scrutiny: "++runPrettyCTerm t++ " cannot be cast to type: "++runPrettyType ty
readTypeError (NoError) = "No monoid error"
readTypeError (TrivTypeError a) = "Type error: "++runPrettyType a++" must be of type Unit."
readTypeError (ConsTypeError a) = "Type error: You cannot use cons with the type: "++runPrettyType a
readTypeError (EmptyTypeError a) = "Type error: "++runPrettyType a++" must be of type List."
readTypeError (ZeroTypeError a) = "Type error: "++runPrettyType a++" must be of type Nat."
readTypeError (SuccTypeError a) = "Type error: "++runPrettyType a++" must be of type Nat to use the successor function."
readTypeError (CastError a b) = "Type error: casting error - the type: "++runPrettyType a++" is inconsistent with type: "++runPrettyType b
readTypeError (BoxTypeError a) = "Type error: cannot box type: "++runPrettyType a
readTypeError (UnboxTypeError a) = "Type error: cannot unbox type: "++runPrettyType a
readTypeError (UnboxError a) = "Type error: unboxing error on type: "++runPrettyType a
readTypeError (BoxError a) = "Type error: boxing error on type: "++runPrettyType a
readTypeError (SquashError a) = "Type error: cannot squash type: "++runPrettyType a
readTypeError (SplitError a) = "Type error: cannot split type: "++runPrettyType a
readTypeError (CastInsertionError a b) = "Type error: cannot cast from type: "++runPrettyType a++" to type: "++runPrettyType b
readTypeError (ListElemTypeMismatch t ty) = "Type error: the term: "++runPrettyTerm t++" has the wrong type of : "++runPrettyType ty++" and cannot be added to the list."
readTypeError (InconsistentTypes a b) = "Type error: you cannot add an element of type: "++runPrettyType a++" to a list with elements of type: "++runPrettyType b
readTypeError (NonFunctionType t ty) = "Type error: The term: "++runPrettyTerm t++" is not a function type and instead is of type "++runPrettyType ty
