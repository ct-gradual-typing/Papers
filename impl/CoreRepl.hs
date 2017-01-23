module Repl where

import Control.Monad.State
import System.Console.Haskeline
import System.Console.Haskeline.MonadException    
import System.Exit
import System.FilePath
import Unbound.LocallyNameless.Subst

import Queue
import CoreSyntax
import TypeSyntax
import CoreParser
import CorePretty
import CoreTypeChecker
import Eval
import TypeErrors

data QDefName = Var CVnm | DefName CVnm
data QDefDef  = VarType Type | DefTerm CTerm

getQDef :: (QDefName, QDefDef) -> REPLStateIO (Either (CVnm, Type) (CVnm, CTerm))
getQDef e@(Var x, VarType ty) = return $ Left (x , ty)
getQDef e@(DefName x, DefTerm t) = return $ Right (x , t)
getQDef _ = error "..."

type Qelm = (CVnm, CTerm)
type REPLStateIO = StateT (FilePath,Queue Qelm) IO

instance MonadException m => MonadException (StateT s m) where
    controlIO f = StateT $ \s -> controlIO $ \(RunIO run) -> let
                    run' = RunIO (fmap (StateT . const) . run . flip runStateT s)
                    in fmap (flip runStateT s) $ f run'
                
io :: IO a -> REPLStateIO a
io i = liftIO i
    
pop :: REPLStateIO (CVnm, CTerm)
pop = get >>= return.headQ.snd

push :: Qelm -> REPLStateIO ()
push t = do
  (f,q) <- get
  put (f,(q `snoc` t))

set_wdir :: FilePath -> REPLStateIO ()
set_wdir wdir = do
  (_,q) <- get
  put (wdir,q)
         
unfoldDefsInTerm :: (Queue Qelm) -> CTerm -> CTerm
unfoldDefsInTerm q t =
    let uq = toListQ $ unfoldQueue q
     in substs uq t

unfoldQueue :: (Queue Qelm) -> (Queue Qelm)
unfoldQueue q = fixQ q emptyQ step
 where
   step e@(x,t) _ r = (mapQ (substDef x t) r) `snoc` e
    where
      substDef :: Name CTerm -> CTerm -> Qelm -> Qelm
      substDef x t (y, t') = (y, subst x t t')
      
containsTerm :: Queue Qelm -> CVnm -> Bool
containsTerm (Queue f r) vnm = (foldl (\b (defName, defTerm)-> b || (vnm == defName)) False r) || (foldl (\b (defName, defTerm)-> b || (vnm == defName)) False f) 

tyCheckQ :: GFile -> REPLStateIO ()
tyCheckQ (Queue [] []) = return () 
tyCheckQ q = do
  (f, defs) <- get
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
                              -- Determine if definition already in queue
                              if(containsTerm defs v)
                              then  io.putStrLn $ "Error: The variable "++(show v)++" is already in the context."
                              else  do
                                push (v,tu)
                                tyCheckQ $ tailQ q
                            else io.putStrLn $ "Error: "++(runPrettyType ity)++" is not a subtype of "++(runPrettyType ty)
    else io.putStrLn $ "Error: free variables found in "++(show v)

handleCMD :: String -> REPLStateIO ()
handleCMD "" = return ()
handleCMD s =    
    case (parseLine s) of
      Left msg -> io $ putStrLn msg
      Right l -> handleLine l
  where
    handleLine :: REPLExpr -> REPLStateIO ()
    handleLine (HelpMenu) = do
      io.putStrLn $ "----------------------------------------------------------"
      io.putStrLn $ "                  The Core Grady Help Menu                     "
      io.putStrLn $ "----------------------------------------------------------"
      io.putStrLn $ ":h/:help -> Display the help menu"
      io.putStrLn $ ":t/:type <term> -> Typecheck a term"
      io.putStrLn $ ":s/:show <term> -> Display the Abstract Syntax Type of a term"
      io.putStrLn $ ":u/:unfold <term> -> Unfold the expression" -- TODO: Is there a better way to explaing this?
      io.putStrLn $ ":d/:dump -> Display the context"
      io.putStrLn $ ":l/:load <filepath> -> Load an external file into the context"
      io.putStrLn $ ":l/:load <filepath> -> Load an external file into the context"
      io.putStrLn $ "let <Variable name> = <expression> -> Bind an expression to a variable and load it into the context"
      io.putStrLn $ "You may also evaluate expressions directly in the Repl"
      io.putStrLn $ "----------------------------------------------------------"
    handleLine (Eval t) = do
      (f, defs) <- get
      let tu = unfoldDefsInTerm defs t
          r = eval tu
       in case r of
            Left m -> io.putStrLn.readTypeError $ m
            Right e -> io.putStrLn.runPrettyCTerm $ e
    handleLine (Let x t) = do
      (f, defs) <- get
      let tu = unfoldDefsInTerm defs t
          r = runIR tu
       in case r of
            Left m -> io.putStrLn.readTypeError $ m
            Right ty ->  do
                if(containsTerm defs x)
                then io.putStrLn $ "error: The variable "++(show x)++" is already in the context."
                else push (x , t)
    handleLine (TypeCheck t) = do
      (_, defs) <- get
      let tu = unfoldDefsInTerm defs t
          r = runIR tu
       in case r of
            Left m -> io.putStrLn.readTypeError $ m
            Right ty ->  io.putStrLn.runPrettyType $ ty
    handleLine (ShowAST t) = do
      (_,defs) <- get
      io.putStrLn.show $ unfoldDefsInTerm defs t
    handleLine (Unfold t) =
        get >>= (\(f,defs) -> io.putStrLn.runPrettyCTerm $ unfoldDefsInTerm defs t)
    handleLine (LoadFile p) = do
      let wdir = takeDirectory p
      let file = takeFileName p
      if (not (null wdir))
      then do
        set_wdir wdir
        loadFile file
      else loadFile file
    handleLine DumpState = get >>= io.print.(mapQ prettyDef).snd
     where
       prettyDef :: (Name a, CTerm) -> String
       prettyDef (x, t) = "let "++(n2s x)++" = "++(runPrettyCTerm t)

loadFile :: FilePath -> REPLStateIO ()
loadFile p = do
  (wdir,_) <- get
  -- delete definitions currently in queue, this allows reloading the same file after making changes
  put (wdir,emptyQ)         
  msgOrGFile <- lift $ runFileParser p wdir
  case msgOrGFile of
    Left l -> io.putStrLn $ l
    Right r -> tyCheckQ r
   
getFV :: CTerm -> [CVnm]
getFV t = fv t :: [CVnm]

banner :: String
banner = "Welcome to Core Grady!\n\n"

main :: IO ()
main = do
  putStr banner
  evalStateT (runInputT defaultSettings loop) ("",emptyQ)
   where 
       loop :: InputT REPLStateIO ()
       loop = do           
           minput <- getInputLine "Core Grady> "
           case minput of
               Nothing -> return ()
               Just [] -> loop
               Just input | input == ":q" || input == ":quit"
                              -> liftIO $ putStrLn "Goodbye!" >> return ()
                          | otherwise -> (lift.handleCMD $ input) >> loop