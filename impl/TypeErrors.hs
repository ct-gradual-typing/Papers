module TypeErrors (module Control.Monad.Reader,
                   module Control.Monad.Except,
                   module Control.Applicative, TypeError(..), readTypeError)where

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.Except  

import Syntax  
import Pretty

--Error Types
data TypeError = FreeVarsError Vnm
               | FreeTVarsError TVnm
               | SuccError Term
               | FstError Term
               | SndError Term
               | FunError Term
               | AppError Type Type
               | TAppError Term Type Type
               | NoTypeError Term
               | UnboxError Type
               | BoxError Type SrcPos
               | UnMatchedTypes Type Type
               | SError Type
               | SubtypeError Type Type
               | TypeMismatch Type Type
               | NotProdType Term Type
               | NotArrowTypeTerm Term Type
               | NotArrowType Type
               | NotForallType Type
               | NotForallTypeTerm Term Type
               | TypeVariableNameMismatch TVnm TVnm
               | SplitTypeError Type SrcPos
               | SquashTypeError Type SrcPos
               | SplitSquashTypeError Type Type SrcPos
               | UnboxBoxTypeError Type Type SrcPos
               | BoxTypeError Type SrcPos
               | UnboxTypeError Type SrcPos
               | NCaseBranchesMistype Type Type
               | LCaseBranchesMistype Type Type
               | LCaseScrutinyTypeError Term Type
               | NoError
               | CtxNotOk
               | TrivTypeError Type
               | ConsTypeError Type
               | EmptyTypeError Type
               | ZeroTypeError Type
               | SuccTypeError Type
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
readTypeError (BoxError a l) = "Type error: You cannot box "++ (runPrettyType a)++" : "++(show l)
readTypeError (UnboxError a) = "Type error: You cannot unbox "++ (runPrettyType a)++", you can only unbox types Nat and Unit"
readTypeError (SError a) = "Type error: "++(runPrettyType a)++" must be of type ? -> ? or (? x ?)"
readTypeError e = show e