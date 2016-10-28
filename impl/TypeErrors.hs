module TypeErrors (module Control.Monad.Reader,
                   module Control.Monad.Except, TypeError(..), readTypeError)where

import Control.Monad.Reader
import Control.Monad.Except  

import Syntax  


--Error Types
data TypeError = FreeVarsError Vnm
               | SuccError Term
               | FstError Term
               | SndError Term
               | FunError Term
               | AppError Type
               | FreshError
               | NoTypeError Term
               | UnboxError Term
               | BoxError Term
               | UnMatchedTypes Type Type
               | SqshError Term
  deriving(Show)
  
readTypeError :: TypeError -> String
readTypeError (FreeVarsError a) =
    "Type error: variable " ++(n2s a) ++ " is free, but I can only typecheck closed terms."
readTypeError (SuccError a) = "Type error (successor)"  -- ++(prettyType a)
readTypeError (FstError a) = "Type error(first projection)"
readTypeError (SndError a) = "Type error (second projection)"
readTypeError (FunError a) = "Type error (function)"
readTypeError (AppError a) = "Type error (application): types don't match"
readTypeError (FreshError) = "Type error: Fresh error"
readTypeError (NoTypeError a) = "Type error: No type was found"  
readTypeError (UnMatchedTypes a b) = "Type error: "++ (show a) ++" and "++ (show b) ++" aren't the same type."
readTypeError (BoxError a) = "Type error: You cannot box "++ (show a)
readTypeError (UnboxError a) = "Type error: You cannont unbox "++ (show a)
readTypeError (SqshError a) = "Type error (squash)"