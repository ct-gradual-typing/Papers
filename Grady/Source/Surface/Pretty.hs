{-# LANGUAGE FlexibleContexts #-}
module Surface.Pretty (module PrettyType, prettyTerm, runPrettyTerm) where

import Surface.Syntax
import PrettyType

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
           
parenTerm :: Term -> (Term -> LFreshM String) -> LFreshM String
parenTerm t@(Var _) f = f t
parenTerm t@Triv f = f t
parenTerm t@Zero f = f t
parenTerm t f = f t

prettyUnaryArg :: Term -> (Term -> LFreshM String) -> String -> LFreshM String
prettyUnaryArg t f c = parenTerm t f >>= (\r -> return $ c++" "++r)
         
prettyTerm :: Term -> LFreshM String
prettyTerm Triv = return "triv"
prettyTerm Zero = return "0"
prettyTerm (Unbox ty) = do
    pTy <- prettyType ty
    return $ "unbox <"++pTy++">"
prettyTerm (Box ty) = do
    pTy <- prettyType ty
    return $ "box <"++pTy++">"
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
     case t' of
       (Cons _ _) -> do
               s' <- consToList t'
               return $ s ++ "," ++ s'
       Empty -> return s
       (TApp _ Empty) -> return s
       _ -> do
         s' <- prettyTerm t'
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
  
runPrettyTerm :: Term -> String
runPrettyTerm = runLFreshM.prettyTerm