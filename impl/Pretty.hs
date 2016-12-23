{-# LANGUAGE FlexibleContexts #-}
module Pretty (prettyType, prettyTerm, runPrettyTerm, runPrettyType) where

import Syntax
import Parser

prettyType :: Type -> String
prettyType Nat = "Nat"
prettyType Unit = "1"
prettyType U = "?"
prettyType (Arr t1 t2) =
    case t1 of
      Arr _ _ -> "("++s1++") -> "++s2
      _ -> s1++" -> "++s2
  where
    s1 = prettyType t1
    s2 = prettyType t2
prettyType (Prod t1 t2) =
    case (t1 , t2) of
      (Arr _ _ , Arr _ _) -> "("++s1++") x ("++s2++")"
      (Arr _ _ , _) -> "("++s1++") x "++s2
      (_ , Arr _ _) -> s1++" x ("++s2++")"                       
      _ -> s1++" x "++s2
  where
    s1 = prettyType t1
    s2 = prettyType t2

parenTerm :: Term -> (Term -> LFreshM String) -> LFreshM String
parenTerm t@(Var _) f = f t
parenTerm t@Triv f = f t
parenTerm t@Zero f = f t             
parenTerm t@Split f = f t
parenTerm t@Squash f = f t        
parenTerm t f = f t >>= (\r -> return $ "("++r++")")

prettyUnaryArg :: Term -> (Term -> LFreshM String) -> String -> LFreshM String
prettyUnaryArg t f c = parenTerm t f >>= (\r -> return $ c++" "++r)
         
prettyTerm :: Term -> LFreshM String
prettyTerm Triv = return "triv"
prettyTerm Zero = return "0"
prettyTerm (Box ty) = let sty = prettyType ty
                       in return $ "box<"++sty++">"
prettyTerm (Unbox ty) = let sty = prettyType ty
                         in return $"unbox<"++sty++">"   
prettyTerm (Var x) = return.n2s $ x
prettyTerm (Fst t) = prettyUnaryArg t prettyTerm "fst"
prettyTerm (Snd t) = prettyUnaryArg t prettyTerm "snd"
prettyTerm (Succ t) = prettyUnaryArg t prettyTerm "succ"
prettyTerm (Fun ty b) = do
  lunbind b $ (\(x,t) -> do           
                 s <- prettyTerm t
                 return $ "\\("++(n2s x)++":"++tyS++")."++s)
 where
   tyS = prettyType ty
prettyTerm (App t1 t2) = do
  s1 <- parenTerm t1 prettyTerm
  s2 <- parenTerm t2 prettyTerm
  return $ s1++" "++s2
prettyTerm (Pair t1 t2) = do
  s1 <- parenTerm t1 prettyTerm
  s2 <- parenTerm t2 prettyTerm
  return $ "("++s1++", "++s2++")"
  
prettyTerm Squash = return "squash"
prettyTerm Split = return "split"

testPretty parser pretty s = do
  let o = parse parser "" s in  
    case o of
      Left e -> error $ show e
      Right r -> runLFreshM (pretty r)

testPrettyTerm :: String -> String
testPrettyTerm = testPretty expr prettyTerm

runPrettyType :: Type -> String
runPrettyType = prettyType
                 
runPrettyTerm :: Term -> String
runPrettyTerm = runLFreshM.prettyTerm