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
import Eval
import TypeErrors

type Qelm = (Vnm, Term)
type REPLStateIO = StateT (Queue Qelm) IO

instance MonadException m => MonadException (StateT s m) where
    controlIO f = StateT $ \s -> controlIO $ \(RunIO run) -> let
                    run' = RunIO (fmap (StateT . const) . run . flip runStateT s)
                    in fmap (flip runStateT s) $ f run'
                
io :: IO a -> REPLStateIO a
io i = liftIO i
    
pop :: REPLStateIO (Vnm, Term)
pop = get >>= return.headQ

push :: Qelm -> REPLStateIO ()
push t = get >>= put.(`snoc` t) 
         
unfoldDefsInTerm :: (Queue Qelm) -> Term -> Term
unfoldDefsInTerm q t =
    let uq = toListQ $ unfoldQueue q
     in substs uq t

unfoldQueue :: (Queue Qelm) -> (Queue Qelm)
unfoldQueue q = fixQ q emptyQ step
 where
   step e@(x,t) _ r = (mapQ (substDef x t) r) `snoc` e
    where
      substDef :: Name Term -> Term -> Qelm -> Qelm
      substDef x t (y, t') = (y, subst x t t')

tyCheckQ :: GFile -> REPLStateIO ()
tyCheckQ (Queue [] []) = return () 
tyCheckQ q = do
  defs <- get
  let term@(Def v ty t) = headQ q
  do 
    -- Unfold each term from queue and see if free variables exist
    let tu = unfoldDefsInTerm defs t
    let numFV = length (getFV tu)
    if (numFV == 0)
    -- TypeCheck term from Prog
    then let r = runIR tu            
          in case r of
               Left err -> io.putStrLn.readTypeError $ err
                    -- Verify type from TypeChecker matches expected type from file
                    -- If it does, add to context (i.e. definition queue)
               Right ity ->
                   do
                      case ity `isSubtype` ty of
                        Left er -> io.putStrLn.readTypeError $ er
                        Right b -> 
                            if b
                            then do
                              push (v,tu)
                              tyCheckQ $ tailQ q
                            else io.putStrLn $ "TODO: make error message"
    else io.putStrLn $ "error - free variables found in q: "++(show q)

handleCMD :: String -> REPLStateIO ()
handleCMD "" = return ()
handleCMD s =    
    case (parseLine s) of
      Left msg -> io $ putStrLn msg
      Right l -> handleLine l
  where
    handleLine :: REPLExpr -> REPLStateIO ()
    handleLine (Eval t) = do
      defs <- get
      let tu = unfoldDefsInTerm defs t
          r = eval tu
       in case r of
            Left m -> io.putStrLn.readTypeError $ m
            Right e -> io.putStrLn.runPrettyTerm $ e
    handleLine (Let x t) = push (x , t)
    handleLine (TypeCheck t) = do
      defs <- get
      let tu = unfoldDefsInTerm defs t
          r = runIR tu
       in case r of
            Left m -> io.putStrLn.readTypeError $ m
            Right ty ->  io.putStrLn.runPrettyType $ ty
    handleLine (ShowAST t) = io.putStrLn.show $ t
    handleLine (Unfold t) = get >>= (\defs -> io.putStrLn.runPrettyTerm $ unfoldDefsInTerm defs t)
    handleLine (LoadFile p) = do
      msgOrGFile <- lift $ runParse p
      case msgOrGFile of
        Left l -> io.putStrLn $ l
        Right r -> tyCheckQ r                                          
    handleLine DumpState = get >>= io.print.(mapQ prettyDef)
     where
       prettyDef :: (Name a, Term) -> String
       prettyDef (x, t) = "let "++(n2s x)++" = "++(runPrettyTerm t)
   
getFV :: Term -> [Vnm]
getFV t = fv t :: [Vnm]

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
               Just [] -> loop
               Just input | input == ":q" || input == ":quit"
                              -> liftIO $ putStrLn "Goodbye!" >> return ()
                          | otherwise -> (lift.handleCMD $ input) >> loop