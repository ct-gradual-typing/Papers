module TypeErrors (module Control.Monad.Reader,
                   module Control.Monad.Except,
                   module Control.Applicative, TypeError(..), readTypeError)where

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.Except  

import Syntax
import CoreSyntax  
import Pretty
import CorePretty

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
    "Type error: variable " ++(n2s a) ++ " is free, but I can only typecheck closed terms."
readTypeError (CoreFreeVarsError a) =
    "Type error: variable " ++(n2s a) ++ " is free, but I can only typecheck closed terms."
readTypeError (FreeTVarsError a) = 
    "Type error: variable " ++(n2s a) ++ " is free, but I can only typecheck closed terms."
readTypeError (SuccError a) = "Type error (successor): "++runPrettyTerm a
readTypeError (FstError a) = "Type error(first projection): "++runPrettyTerm a
readTypeError (FstTypeError a) = "Type error: "++runPrettyType a
readTypeError (SndTypeError a) = "Type error: "++runPrettyType a
readTypeError (SndError a) = "Type error (second projection): "++runPrettyTerm a
readTypeError (FunError a) = "Type error (function): "++runPrettyTerm a
readTypeError (AppError a b) = "Type error (application): types don't match " ++runPrettyType a++" !~ "++(runPrettyType b)
readTypeError (TAppError a b c) = "Type error: the term: "++runPrettyTerm a++" does not have the correct types " ++runPrettyType b++" and "++(runPrettyType c)
readTypeError (NoTypeError a) = "Type error: No type (" ++runPrettyTerm a++ ") was found"  
readTypeError (UnMatchedTypes a b) = "Type error: "++ (runPrettyType a) ++" must have the correct type with "++ (runPrettyType b)
readTypeError (SError a) = "Type error: "++(runPrettyType a)++" must be of type ? -> ? or (? x ?)"
readTypeError (SubtypeError a b) = "Type error: "++runPrettyType a++"is not a subtype of "++runPrettyType b
readTypeError (TypeMismatch a b) = "Type error: "++runPrettyType a++"is a mismatched type with "++runPrettyType b
readTypeError (NotProdTypeTerm t ty) = "Type error: "++runPrettyTerm t++ "is not a product type of "++runPrettyType ty
readTypeError (CoreNotProdTypeTerm t ty) = "Type error: "++runPrettyCTerm t++ "is not a product type of "++runPrettyType ty
readTypeError (NotArrowTypeTerm t ty) = "Type error: "++runPrettyTerm t++ "is not a function type of "++runPrettyType ty
readTypeError (CoreNotArrowTypeTerm t ty) = "Type error: "++runPrettyCTerm t++ "is not a function type of "++runPrettyType ty
readTypeError (NotArrowType a) = "Type error: "++runPrettyType a++ "is not a function type"
readTypeError (NotForallType a) = "Type error: "++runPrettyType a++ "is not a for all type"
readTypeError (SplitSquashTypeError a b) = "Type error: split or squash error on "++runPrettyType a++ " and "++runPrettyType b 
readTypeError (UnboxBoxTypeError a b) = "Type error: unbox error on "++runPrettyType a++ " and "++runPrettyType b 
readTypeError (NotNatType a) = "Type error: "++runPrettyType a++ " is not of type Nat"
readTypeError (NotListType a) = "Type error: "++runPrettyType a++ " is not of type List"
readTypeError (NotProdType a) = "Type error: "++runPrettyType a++ " is not of type Product"
readTypeError (NotForallTypeTerm t ty) = "Type error: the term "++runPrettyTerm t++ " is not of type "++runPrettyType ty
readTypeError (CoreNotForallTypeTerm t ty) = "Type error: the term "++runPrettyCTerm t++ " is not of type "++runPrettyType ty
readTypeError (TypeVariableNameMismatch a b) = "Type error: "++n2s a++ " needs to match "++n2s b
readTypeError (NCaseBranchesMistype a b) = "Type error: Case error, "++runPrettyType a++ " does not match "++runPrettyType b
readTypeError (LCaseBranchesMistype a b) = "Type error: Case error, "++runPrettyType a++ " does not match "++runPrettyType b
readTypeError (LCaseScrutinyTypeError t ty) = "Type error: The scrutiny: "++runPrettyTerm t++ " of type: "++runPrettyType ty++" should have a different type"
readTypeError (CoreLCaseScrutinyTypeError t ty) = "Type error: The scrutiny: "++runPrettyCTerm t++ " of type: "++runPrettyType ty++" should have a different type"
readTypeError (NoError) = "No error"
readTypeError (CtxNotOk) = "Type error: The context is off somehow"
readTypeError (TrivTypeError a) = "Type error: Triv type error on "++runPrettyType a
readTypeError (ConsTypeError a) = "Type error: Cons type error on "++runPrettyType a
readTypeError (EmptyTypeError a) = "Type error: Empty type error on "++runPrettyType a
readTypeError (ZeroTypeError a) = "Type error: Zero type error on "++runPrettyType a
readTypeError (SuccTypeError a) = "Type error: Successor type error on "++runPrettyType a
readTypeError (CastError a b) = "Type error: casting error from type: "++runPrettyType a++" to "++runPrettyType b
readTypeError (BoxTypeError a) = "Type error: boxing type error on type: "++runPrettyType a
readTypeError (UnboxTypeError a) = "Type error: unboxing typing error on type: "++runPrettyType a
readTypeError (UnboxError a) = "Type error: unboxing error on type: "++runPrettyType a
readTypeError (BoxError a) = "Type error: boxing error on type: "++runPrettyType a
readTypeError (SquashError a) = "Type error: squash error on type: "++runPrettyType a
readTypeError (SplitError a) = "Type error: split error on type: "++runPrettyType a
readTypeError (CastInsertionError a b) = "Type error: cast insertion error on types: "++runPrettyType a++" and "++runPrettyType b
readTypeError (ListElemTypeMismatch t ty) = "Type error: the list element: "++runPrettyTerm t++"has the wrong type of : "++runPrettyType ty
readTypeError (InconsistentTypes a b) = "Type error: inconsistent types: "++runPrettyType a++" !~ "++runPrettyType b
readTypeError (NonFunctionType t ty) = "Type error: The term: "++runPrettyTerm t++" is not a function type and instead is on type "++runPrettyType ty
