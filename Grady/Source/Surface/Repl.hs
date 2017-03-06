{-# LANGUAGE TypeSynonymInstances,
             FlexibleInstances #-}
module Surface.Repl where

import Control.Monad.State
import System.Console.Haskeline
import System.Console.Haskeline.MonadException    
import System.Exit
import System.FilePath
import Unbound.LocallyNameless.Subst

import Queue
import Surface.Syntax
import Core.Syntax
import Surface.Parser
import Surface.Pretty
import Core.Pretty
import Surface.TypeChecker
import Core.Eval
import TypeErrors
import CastInsertion

type Qelm = (Vnm, Term)
type REPLStateIO = StateT (FilePath,Queue Qelm) IO

instance MonadException m => MonadException (StateT (FilePath,Queue Qelm) m) where
    controlIO f = StateT $ \s -> controlIO $ \(RunIO run) -> let
                    run' = RunIO (fmap (StateT . const) . run . flip runStateT s)
                    in fmap (flip runStateT s) $ f run'
                
io :: IO a -> REPLStateIO a
io i = liftIO i
    
pop :: REPLStateIO (Vnm, Term)
pop = get >>= return.headQ.snd

push :: Qelm -> REPLStateIO ()
push t = do
  (f,q) <- get
  put (f,(q `snoc` t))

set_wdir :: FilePath -> REPLStateIO ()
set_wdir wdir = do
  (_,q) <- get
  put (wdir,q)
         
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
      
containsTerm :: Queue Qelm -> Vnm -> Bool
containsTerm (Queue f r) vnm = foldl (\b (defName, defTerm)-> b || (vnm == defName)) False f

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
                            then
                              -- Determine if definition already in queue
                              if(containsTerm defs v)
                              then  io.putStrLn $ "error: The variable "++(show v)++" is already in the context."
                              else  do
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
    handleLine (Let x t) = do
      (f, defs) <- get
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
        get >>= (\(f,defs) -> io.putStrLn.runPrettyTerm $ unfoldDefsInTerm defs t)
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
       prettyDef :: (Name a, Term) -> String
       prettyDef (x, t) = "let "++(n2s x)++" = "++(runPrettyTerm t)
    handleLine (Eval t) = do
      (_, defs) <- get
      let tu = unfoldDefsInTerm defs t
      let r = insertCasts tu
      case r of
        Left m -> io.putStrLn.readTypeError $ m
        Right (ct, _) -> let me = eval ct
                          in case me of
                               Left m' -> io.putStrLn.readTypeError $ m'
                               Right e -> io.putStrLn.runPrettyCTerm $ e
      
loadFile :: FilePath -> REPLStateIO ()
loadFile p = do
  (wdir,_) <- get
  -- delete definitions currently in queue, this allows reloading the same file after making changes
  put (wdir,emptyQ)         
  msgOrGFile <- lift $ runFileParser p wdir
  case msgOrGFile of
    Left l -> io.putStrLn $ l
    Right r -> tyCheckQ r
   
getFV :: Term -> [Vnm]
getFV t = fv t :: [Vnm]

helpMenu :: String                          
helpMenu = 
      "-----------------------------------------------------------------------------------\n"++
      "                  The Surface Grady Help Menu                                      \n"++
      "-----------------------------------------------------------------------------------\n"++
      ":help             (:h)  Display the help menu\n"++
      ":type term        (:t)  Typecheck a term\n"++
      ":show <term>      (:s)  Display the Abstract Syntax Type of a term\n"++
      ":unfold <term>    (:u)  Unfold the expression into one without toplevel definition.\n"++ 
      ":dump             (:d)  Display the context\n"++
      "load <filepath>   (:l)  Load an external file into the context\n"++
      "-----------------------------------------------------------------------------------"

repl :: IO ()
repl = do
  evalStateT (runInputT defaultSettings loop) ("",emptyQ)
   where 
       loop :: InputT REPLStateIO ()
       loop = do           
           minput <- getInputLine "Surface> "
           case minput of
               Nothing -> return ()
               Just [] -> loop
               Just input | input == ":q" || input == ":quit"
                              -> liftIO $ putStrLn "Leaving Surface Grady." >> return ()
                          | input == ":h" || input == ":help"
                              -> (liftIO $ putStrLn helpMenu) >> loop
                          | otherwise -> (lift.handleCMD $ input) >> loop
                          