module TypeErrors (module Control.Monad.Reader,
                   module Control.Monad.Except,
                   module Control.Applicative, TypeError(..), readTypeError)where

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.Except  

import Syntax
import CoreSyntax  
import Pretty

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
readTypeError (SuccError a) = "Type error (successor): "++runPrettyTerm a
readTypeError (FstError a) = "Type error(first projection): "++runPrettyTerm a
readTypeError (SndError a) = "Type error (second projection): "++runPrettyTerm a
readTypeError (FunError a) = "Type error (function): "++runPrettyTerm a
readTypeError (AppError a b) = "Type error (application): types don't match " ++runPrettyType a++" !~ "++(runPrettyType b)
readTypeError (NoTypeError a) = "Type error: No type (" ++runPrettyTerm a++ ") was found"  
readTypeError (UnMatchedTypes a b) = "Type error: "++ (runPrettyType a) ++" must have the correct type with "++ (runPrettyType b)
readTypeError (SError a) = "Type error: "++(runPrettyType a)++" must be of type ? -> ? or (? x ?)"
readTypeError e = show e