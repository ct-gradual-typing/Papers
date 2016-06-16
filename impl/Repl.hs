module Repl where

import Control.Monad.State    
    
import Syntax
import Parser

type REPLStateIO a = StateT [REPLExpr] IO a

io :: IO a -> REPLStateIO a
io i = liftIO $ i
    
pop :: REPLStateIO (Vnm, Term)
pop = get >>= io.caseLet.head 
 where
   caseLet :: REPLExpr -> IO (Vnm, Term)
   caseLet (Let n t) = return $ (n , t)
   caseLet _ = fail "Internal state is not consistent: state should contain only top-level definitions."

push :: REPLExpr -> REPLStateIO ()
push (t@(Let _ _)) = get >>= put.(t:) 
push _ = io $ fail "Tried to push non-toplevel definition."

