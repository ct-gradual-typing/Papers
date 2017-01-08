{-# LANGUAGE FlexibleContexts #-}
module Pretty (prettyType, prettyTerm, runPrettyTerm, runPrettyType) where

import Syntax

prettyType :: Type -> LFreshM String
prettyType (TVar x) = return.n2s $ x
prettyType Nat = return "Nat"
prettyType Unit = return "Unit"
prettyType Simple = return "Simple"
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
prettyType (Forall ty b) =
    lunbind b $ (\(x,ty') ->
       prettyType ty >>= (\s1 -> prettyType ty' >>= (\s2 -> return $ "forall ("++(n2s x)++"<:"++s1++")."++"("++s2++")")))
prettyType (List ty) = do
  s <- prettyType ty
  return $ "["++s++"]"
  
isInt :: Term -> Bool
isInt Zero = True
isInt (Succ t) = isInt t
isInt _ = False

term2int' :: Term -> Integer
term2int' Zero = 0
term2int' (Succ t) = 1 + (term2int' t)

term2int :: Term -> Maybe Integer
term2int t | isInt t = Just $ term2int' t
           | otherwise = Nothing
           
parenType :: Type -> (Type -> LFreshM String) -> LFreshM String
parenType ty@(TVar _) f = f ty
parenType ty@Nat f = f ty
parenType ty@Unit f = f ty
parenType ty@Simple f = f ty
parenType ty@Top f = f ty
parenType ty@U f = f ty
parenType ty@(Arr t1 t2) f = f ty
parenType ty@(Prod t1 t2) f = f ty
parenType ty@(Forall ty' b) f = f ty >>= (\r -> return $ "("++r++")")
parenType ty@(List ty') f = f ty 

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
prettyTerm (Succ t) =
    case (term2int t) of
      Just n -> return.show $ n+1
      Nothing -> (prettyTerm t) >>= (\s -> return $ "succ "++s)
  
prettyTerm (NCase t t1 b) = do
  s <- prettyTerm t
  s1 <- prettyTerm t1
  lunbind b $ (\(x,t2) ->
   do
     s2 <- prettyTerm t2
     return $ "ncase "++s++" of "++s1++" || "++(n2s x)++"."++s2)
prettyTerm (Box ty) = do
  sty <- prettyType ty
  return $ "box<"++sty++">"
prettyTerm (Unbox ty) = do
  sty <- prettyType ty
  return $"unbox<"++sty++">"   
prettyTerm (Var x) = return.n2s $ x
prettyTerm (Fst t) = prettyUnaryArg t prettyTerm "fst"
prettyTerm (Snd t) = prettyUnaryArg t prettyTerm "snd"
prettyTerm Empty = return "[]"
prettyTerm t@(Cons _ _) = do
  s <- consToList t
  return $ "["++s++"]"
 where
   consToList :: Term -> LFreshM String
   consToList Empty = return ""
   consToList (Cons h' Empty) = prettyTerm h'
   consToList (Cons h' t') = do
     s <- prettyTerm h'
     s' <- consToList t'
     return $ s ++ "," ++ s'
   consToList t = prettyTerm t
prettyTerm (LCase t t1 b) = do
  s <- prettyTerm t
  s1 <- prettyTerm t1
  lunbind b $ (\(x,b') ->
     lunbind b' $ (\(y,t2) ->
   do
     s2 <- prettyTerm t2
     return $ "lcase "++s++" of "++s1++" || "++(n2s x)++","++(n2s y)++"."++s2))
prettyTerm (Fun ty b) = do
  tyS <- prettyType ty
  lunbind b $ (\(x,t) -> do           
                 s <- prettyTerm t
                 return $ "\\("++(n2s x)++":"++tyS++")."++s)
prettyTerm (TFun ty b) = do
  lunbind b $ (\(x,t) -> do
                 sy <- prettyType ty
                 s <- prettyTerm t
                 return $ "\\("++(n2s x)++"<:"++sy++")."++s)   
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