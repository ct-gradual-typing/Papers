{-# LANGUAGE FlexibleContexts #-}
module Core.Pretty (module PrettyType, prettyCTerm, runPrettyCTerm) where

import Core.Syntax
import PrettyType
  
isCInt :: CTerm -> Bool
isCInt CZero = True
isCInt (CSucc t) = isCInt t
isCInt _ = False

cterm2int' :: CTerm -> Integer
cterm2int' CZero = 0
cterm2int' (CSucc t) = 1 + (cterm2int' t)

cterm2int :: CTerm -> Maybe Integer
cterm2int t | isCInt t = Just $ cterm2int' t
           | otherwise = Nothing
           
parenCTerm :: CTerm -> (CTerm -> LFreshM String) -> LFreshM String
parenCTerm t@(CVar _) f = f t
parenCTerm t@(CSplit ty) f = f t
parenCTerm t@(CSquash ty) f = f t
parenCTerm t@(CBox ty) f = f t
parenCTerm t@(CUnbox ty) f = f t
parenCTerm t@(CSucc ty) f = f t                                  
parenCTerm t@CTriv f = f t
parenCTerm t@CZero f = f t             
parenCTerm t f = f t >>= (\r -> return $ "("++r++")")

prettyUnaryArg :: CTerm -> (CTerm -> LFreshM String) -> String -> LFreshM String
prettyUnaryArg t f c = parenCTerm t f >>= (\r -> return $ c++" "++r)
         
prettyCTerm :: CTerm -> LFreshM String
prettyCTerm CTriv = return "triv"
prettyCTerm CZero = return "0"
prettyCTerm (CSucc t) =
    case (cterm2int t) of
      Just n -> return.show $ n+1
      Nothing -> (prettyCTerm t) >>= (\s -> return $ "succ "++s)

prettyCTerm (CBox ty) = do
  sty <- prettyType ty
  return $ "box<"++sty++">"
prettyCTerm (CUnbox ty) = do
  sty <- prettyType ty
  return $"unbox<"++sty++">"   

prettyCTerm (CSquash ty) = do
  sty <- prettyType ty
  return $ "squash<"++sty++">"
prettyCTerm (CSplit ty) = do
  sty <- prettyType ty
  return $ "split<"++sty++">"
  
prettyCTerm (CNCase t t1 b) = do
  s <- prettyCTerm t
  s1 <- prettyCTerm t1
  lunbind b $ (\(x,t2) ->
   do
     s2 <- prettyCTerm t2
     return $ "ncase "++s++" of "++s1++" || "++(n2s x)++"."++s2)
prettyCTerm (CVar x) = return.n2s $ x
prettyCTerm (CFst t) = prettyUnaryArg t prettyCTerm "fst"
prettyCTerm (CSnd t) = prettyUnaryArg t prettyCTerm "snd"
prettyCTerm CEmpty = return "[]"
prettyCTerm t@(CCons _ _) = do
  s <- consToList t
  return $ "["++s++"]"
 where
   consToList :: CTerm -> LFreshM String
   consToList CEmpty = return ""
   consToList (CCons h' CEmpty) = prettyCTerm h'
   consToList (CCons h' t') = do
     s <- prettyCTerm h'
     case t' of
       (CCons _ _) -> do
               s' <- consToList t'
               return $ s ++ "," ++ s'
       CEmpty -> return s
       (CTApp _ CEmpty) -> return s
       _ -> do
         s' <- prettyCTerm t'
         return $ s ++ "," ++ s'
   consToList t = prettyCTerm t
prettyCTerm (CLCase t ty t1 b) = do
  s <- prettyCTerm t
  s1 <- prettyCTerm t1
  lunbind b $ (\(x,b') ->
     lunbind b' $ (\(y,t2) ->
   do
     s2 <- prettyCTerm t2
     s3 <- prettyType ty
     return $ "lcase "++s++" with type ["++s3++"] of "++s1++"|| "++(n2s x)++","++(n2s y)++"."++s2))
prettyCTerm (CFun ty b) = do
  tyS <- prettyType ty
  lunbind b $ (\(x,t) -> do           
                 s <- prettyCTerm t
                 return $ "\\("++(n2s x)++":"++tyS++")."++s)
prettyCTerm (CTFun ty b) = do
  lunbind b $ (\(x,t) -> do
                 sy <- prettyType ty
                 s <- prettyCTerm t
                 return $ "\\("++(n2s x)++"<:"++sy++")."++s)   
prettyCTerm (CApp t1 t2) = do
  s1 <- parenCTerm t1 prettyCTerm
  s2 <- parenCTerm t2 prettyCTerm
  return $ s1++" "++s2
prettyCTerm (CTApp ty t) = do
  s <- parenCTerm t prettyCTerm
  sy <- prettyType ty
  return $ "["++sy++"] "++s
prettyCTerm (CPair t1 t2) = do
  s1 <- parenCTerm t1 prettyCTerm
  s2 <- parenCTerm t2 prettyCTerm
  return $ "("++s1++", "++s2++")"
                 
runPrettyCTerm :: CTerm -> String
runPrettyCTerm = runLFreshM.prettyCTerm