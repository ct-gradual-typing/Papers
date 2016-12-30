{-# LANGUAGE FlexibleContexts #-}
module Pretty (prettyType, prettyTerm, runPrettyTerm, runPrettyType) where

import Syntax

prettyType :: Type -> LFreshM String
prettyType (TVar x) = return.n2s $ x
prettyType Nat = return "Nat"
prettyType Unit = return "Unit"
prettyType Castable = return "Cast"
prettyType Top = return "*"  
prettyType U = return "?"
prettyType (Arr t1 t2) =
    prettyType t1 >>= (\s1 -> prettyType t2 >>= (\s2 ->
    return $ case t1 of
               Arr _ _ -> "("++s1++") -> "++s2
               _ -> s1++" -> "++s2))
prettyType (Prod t1 t2) =
    prettyType t1 >>= (\s1 -> prettyType t2 >>= (\s2 ->
    return $ "("++s1++","++s2++")"))
  where
    s1 = prettyType t1
    s2 = prettyType t2
prettyType (Forall ty b) =
    lunbind b $ (\(x,ty') ->
       prettyType ty >>= (\s1 -> prettyType ty' >>= (\s2 -> return $ "forall ("++(n2s x)++":>"++s1++")."++s2)))

parenTerm :: Term -> (Term -> LFreshM String) -> LFreshM String
parenTerm t@(Var _) f = f t
parenTerm t@Triv f = f t
parenTerm t@Zero f = f t             
parenTerm t@(Split ty) f = f t
parenTerm t@(Squash ty) f = f t        
parenTerm t f = f t >>= (\r -> return $ "("++r++")")

prettyUnaryArg :: Term -> (Term -> LFreshM String) -> String -> LFreshM String
prettyUnaryArg t f c = parenTerm t f >>= (\r -> return $ c++" "++r)
         
prettyTerm :: Term -> LFreshM String
prettyTerm Triv = return "triv"
prettyTerm Zero = return "0"
prettyTerm (Box ty) = do
  sty <- prettyType ty
  return $ "box<"++sty++">"
prettyTerm (Unbox ty) = do
  sty <- prettyType ty
  return $"unbox<"++sty++">"   
prettyTerm (Var x) = return.n2s $ x
prettyTerm (Fst t) = prettyUnaryArg t prettyTerm "fst"
prettyTerm (Snd t) = prettyUnaryArg t prettyTerm "snd"
prettyTerm (Succ t) = prettyUnaryArg t prettyTerm "succ"
prettyTerm (Fun ty b) = do
  tyS <- prettyType ty
  lunbind b $ (\(x,t) -> do           
                 s <- prettyTerm t
                 return $ "\\("++(n2s x)++":"++tyS++")."++s)
prettyTerm (TFun ty b) = do
  lunbind b $ (\(x,t) -> do
                 sy <- prettyType ty
                 s <- prettyTerm t
                 return $ "\\("++(n2s x)++":>"++sy++")."++s)   
prettyTerm (App t1 t2) = do
  s1 <- parenTerm t1 prettyTerm
  s2 <- parenTerm t2 prettyTerm
  return $ s1++" "++s2
prettyTerm (TApp ty t) = do
  s <- parenTerm t prettyTerm
  sy <- prettyType ty
  return $ "["++sy++"] "++s
prettyTerm (Pair t1 t2) = do
  s1 <- parenTerm t1 prettyTerm
  s2 <- parenTerm t2 prettyTerm
  return $ "("++s1++", "++s2++")"
  
prettyTerm (Squash ty) = do
  sty <- prettyType ty
  return $ "squash<"++sty++">"
prettyTerm (Split ty) = do
  sty <- prettyType ty
  return $ "split<"++sty++">"

runPrettyType :: Type -> String
runPrettyType = runLFreshM.prettyType
                 
runPrettyTerm :: Term -> String
runPrettyTerm = runLFreshM.prettyTerm