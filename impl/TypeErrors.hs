module TypeErrors (module Control.Monad.Reader,
                   module Control.Monad.Except, TypeError(..), readTypeError)where

import Control.Monad.Reader
import Control.Monad.Except  

import Syntax  
import Pretty


--Error Types
data TypeError = FreeVarsError Vnm
               | SuccError Term
               | FstError Term
               | SndError Term
               | FunError Term
               | AppError Type Type
               | NoTypeError Term
               | UnboxError Type
               | BoxError Type
               | UnMatchedTypes Type Type
  deriving(Show)
  
readTypeError :: TypeError -> String
readTypeError (FreeVarsError a) =
    "Type error: variable " ++(n2s a) ++ " is free, but I can only typecheck closed terms."
readTypeError (SuccError a) = do
                                pt <- prettyTerm a
                                "Type error (successor): " ++pt
readTypeError (FstError a) = "Type error(first projection)"
readTypeError (SndError a) = "Type error (second projection)"
readTypeError (FunError a) = "Type error (function)"
readTypeError (AppError a b) = "Type error (application): types don't match " ++prettyType a++" !~ "++(prettyType b)
readTypeError (NoTypeError a) = "Type error: No type was found"  
readTypeError (UnMatchedTypes a b) = "Type error: "++ (prettyType a) ++" must have the correct type with "++ (prettyType b)
readTypeError (BoxError a) = "Type error: You cannot box "++ (prettyType a)++", you can only box terminating types"
readTypeError (UnboxError a) = "Type error: You cannont unbox "++ (prettyType a)++", you can only unbox terminating types"

-- readTypeError (SuccError a) = do
                        -- pt <- prettyTerm a
                        -- "Type error (successor): " ++pt 