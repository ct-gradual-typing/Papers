module Repl where

import Control.Monad.State
import System.Console.Haskeline
import System.Console.Haskeline.MonadException    
import System.Exit    

import qualified Queue as Q
import Syntax
import Parser
import Pretty
import TypeChecker
            
type REPLStateIO = StateT (Q.Queue REPLExpr) IO

instance MonadException m => MonadException (StateT s m) where
    controlIO f = StateT $ \s -> controlIO $ \(RunIO run) -> let
                    run' = RunIO (fmap (StateT . const) . run . flip runStateT s)
                    in fmap (flip runStateT s) $ f run'
    
io :: IO a -> REPLStateIO a
io i = liftIO $ i
    
pop :: REPLStateIO (Vnm, Term)
pop = get >>= io.caseLet.(Q.headQ)
 where
   caseLet :: REPLExpr -> IO (Vnm, Term)
   caseLet (Let n t) = return $ (n , t)
   caseLet _ = fail "Error: internal state is not consistent: state should contain only top-level definitions."

push :: REPLExpr -> REPLStateIO ()
push (t@(Let _ _)) = get >>= put.(`Q.snoc` t) 
push _ = io $ fail "Error: tried to push non-toplevel definition."


unfoldDefsInTerm :: (Q.Queue REPLExpr) -> Term -> Term
unfoldDefsInTerm q t = undefined

handleCMD :: String -> REPLStateIO ()
handleCMD s =
    case (parseLine s) of
      Left msg -> io $ putStrLn msg
      Right l -> handleLine l
  where
    handleLine :: REPLExpr -> REPLStateIO ()
    handleLine (t@(Let _ _)) = push t
    handleLine (TypeCheck t) = let ty = typeCheck t  -- unfold top-level definitions first.
                                in io $ putStrLn "Type checking: not implemented yet."
    handleLine (ShowAST t) = io $ putStrLn.show $ t
    handleLine (Unfold t) = get >>= (\defs -> io.putStrLn.runPrettyTerm $ unfoldDefsInTerm defs t)
    handleLine DumpState = get >>= io.print

banner :: String
banner = "Welcome to Grady!\n\nThis is the gradual typing from a categorical perspective repl.\n\n"
                             
main :: IO ()
main = do
  putStr banner
  evalStateT (runInputT defaultSettings loop) Q.emptyQ
   where 
       loop :: InputT REPLStateIO ()
       loop = do           
           minput <- getInputLine "Grady> "
           case minput of
               Nothing -> return ()
               Just input | input == ":q" || input == ":quit" -> lift.io $ putStrLn "Goodbye!" >> return ()
                          | otherwise -> (lift.handleCMD $ input) >> loop