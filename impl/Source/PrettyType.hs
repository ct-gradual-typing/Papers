module PrettyType where

import TypeSyntax

parenType :: Type -> (Type -> LFreshM String) -> LFreshM String
parenType ty@(TVar _) f = f ty
parenType ty@Nat f = f ty
parenType ty@Unit f = f ty
parenType ty@Simple f = f ty
parenType ty@U f = f ty
parenType ty@(Arr t1 t2) f = f ty
parenType ty@(Prod t1 t2) f = f ty
parenType ty@(Forall ty' b) f = f ty >>= (\r -> return $ "("++r++")")
parenType ty@(List ty') f = f ty 

prettyType :: Type -> LFreshM String
prettyType (TVar x) = return.n2s $ x
prettyType Nat = return "Nat"
prettyType Unit = return "Unit"
prettyType Simple = return "Simple"
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

runPrettyType :: Type -> String
runPrettyType = runLFreshM.prettyType