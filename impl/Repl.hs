module Repl where

import Control.Monad.State
import System.Console.Haskeline
import System.Console.Haskeline.MonadException    
import System.Exit    
import Unbound.LocallyNameless.Subst

import Queue
import Syntax
import Parser
import Pretty
import TypeChecker
            
type REPLStateIO = StateT (Queue REPLExpr) IO

instance MonadException m => MonadException (StateT s m) where
    controlIO f = StateT $ \s -> controlIO $ \(RunIO run) -> let
                    run' = RunIO (fmap (StateT . const) . run . flip runStateT s)
                    in fmap (flip runStateT s) $ f run'

-- Error messages:
inconsistentState = "Error: internal state is not consistent: state should contain only top-level definitions."                       
io :: IO a -> REPLStateIO a
io i = liftIO i
    
pop :: REPLStateIO (Vnm, Term)
pop = get >>= io.caseLet.(headQ)
 where
   caseLet :: REPLExpr -> IO (Vnm, Term)
   caseLet (Let n t) = return $ (n , t)
   caseLet _ = fail inconsistentState

push :: REPLExpr -> REPLStateIO ()
push (t@(Let _ _)) = get >>= put.(`snoc` t) 
push _ = io $ fail inconsistentState
         
unfoldDefsInTerm :: (Queue REPLExpr) -> Term -> Term
unfoldDefsInTerm q t =
    let uq = toListQ $ mapQ toPair (unfoldQueue q)
     in substs uq t
 where
   toPair (Let x t) = (x , t)
   toPair _ = error inconsistentState

unfoldQueue :: (Queue REPLExpr) -> (Queue REPLExpr)
unfoldQueue q = fixQ q emptyQ step
 where
   step e@(Let x t) _ r = (mapQ (substREPLExpr x t) r) `snoc` e
    where
      substREPLExpr :: Name Term -> Term -> REPLExpr -> REPLExpr
      substREPLExpr x t (Let y t') = Let y $ subst x t t'
   step _ _ _ = error inconsistentState

handleCMD :: String -> REPLStateIO ()
handleCMD "" = return ()
handleCMD s =    
    case (parseLine s) of
      Left msg -> io $ putStrLn msg
      Right l -> handleLine l
  where
    handleLine :: REPLExpr -> REPLStateIO ()
    handleLine (t@(Let _ _)) = push t
    handleLine (TypeCheck t) = get >>=
                                 (\defs -> let ty = typeCheck.unfoldDefsInTerm defs $ t
                                           in io $ putStrLn "Type checking: not implemented yet.")
    handleLine (ShowAST t) = io.putStrLn.show $ t
    handleLine (Unfold t) = get >>= (\defs -> io.putStrLn.runPrettyTerm $ unfoldDefsInTerm defs t)
    handleLine DumpState = get >>= io.print.(mapQ prettyREPLExpr)
     where
       prettyREPLExpr (Let x t) = "let "++(n2s x)++" = "++(runPrettyTerm t)
       prettyREPLExpr _ = inconsistentState

banner :: String
banner = "Welcome to Grady!\n\nThis is the gradual typing from a categorical perspective repl.\n\n"
                             
main :: IO ()
main = do
  putStr banner
  evalStateT (runInputT defaultSettings loop) emptyQ
   where 
       loop :: InputT REPLStateIO ()
       loop = do           
           minput <- getInputLine "Grady> "
           case minput of
               Nothing -> return ()
               Just input | input == ":q" || input == ":quit" -> liftIO $ putStrLn "Goodbye!" >> return ()
                          | otherwise -> (lift.handleCMD $ input) >> loop