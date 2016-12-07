module Squashing where

import Syntax
import Parser
import Pretty

squashing :: Type -> Term -> Maybe (LFreshM Term)
squashing ty t | is_skeleton ty = Just $ squashing' ty t
               | otherwise = Nothing

-- Convert t of type ty (a skeleton) into a term of type U.
squashing' :: Type -> Term -> LFreshM Term
squashing' U t = return t
squashing' (Arr U U) t = return $ (App Squash t)   -- Squash needs type annotation
squashing' (Arr a b) t = do
  y <- lfresh $ s2n "y"
  t' <- splitting' a (Var y)       
  t'' <- squashing' b (App t t')       
  return $ App Squash $ Fun U $ bind y t''         -- Squash needs type annotation
squashing' (Prod U U) t = return $ (App Squash t)  -- Squash needs type annotation
squashing' (Prod a b) t = do
  t' <- squashing' a (Fst t)
  t'' <- squashing' b (Snd t)
  return $ App Squash $ Pair t' t''                -- Squash needs type annotation
  
squashing' s t = error $ (prettyType s)++" is not a skeleton of the term "++(runPrettyTerm t)

-- Convert t of type U into a term of type ty (a skeleton).
splitting :: Type -> Term -> Maybe (LFreshM Term)
splitting ty t | is_skeleton ty = Just $ splitting' ty t
               | otherwise = Nothing

splitting' :: Type -> Term -> LFreshM Term
splitting' U t = return $ t
splitting' (Arr U U) t = return $ App Split t  -- Split needs type annotation
splitting' (Arr a b) t = do
  y <- lfresh $ s2n "y"
  t' <- squashing' a (Var y)
  t'' <- splitting' b (App (App Split t) t')
  return $ Fun a $ bind y t''
splitting' (Prod U U) t = return $ App Split t -- Split needs type annotation
splitting' (Prod a b) t = do
  t' <- splitting' a (Fst (App Split t))       -- Split needs type annotation
  t'' <- splitting' b (Snd (App Split t))      -- Split needs type annotation
  return $ Pair t' t''

splitting' s t = error $ (prettyType s)++" is not a skeleton that the term "++(runPrettyTerm t)++" can be converted into"

test_squashing :: String -> String -> String
test_squashing sty str = case (q,r) of
                        (Right ty, Right t) -> let Just b = squashing ty t
                                                in runLFreshM $ prettyTerm $ runLFreshM b 
                        (Left m, Right _) -> m
                        (Right _, Left m) -> m
                        (Left m1, Left m2) -> m1 ++ "\n" ++ m2
 where
   r = parseTerm str
   q = parseType sty

test_splitting :: String -> String -> String
test_splitting sty str = case (q,r) of
                        (Right ty, Right t) -> let Just b = splitting ty t
                                                in runLFreshM $ prettyTerm $ runLFreshM b 
                        (Left m, Right _) -> m
                        (Right _, Left m) -> m
                        (Left m1, Left m2) -> m1 ++ "\n" ++ m2
 where
   r = parseTerm str
   q = parseType sty

test_squashing_splitting :: String -> String -> String
test_squashing_splitting sty str = case (q,r) of
                        (Right ty, Right t) -> let Just b =  squashing ty t
                                                   Just b' = splitting ty $ runLFreshM b
                                                in runLFreshM $ prettyTerm $ runLFreshM b'
                        (Left m, Right _) -> m
                        (Right _, Left m) -> m
                        (Left m1, Left m2) -> m1 ++ "\n" ++ m2
 where
   r = parseTerm str
   q = parseType sty

-- Tests:
test1 = test_squashing "(? -> ?) -> (? -> ?)" "x"
test2 = test_squashing "(? -> ?) -> ?" "x"
test3 = test_squashing "? -> (? -> ?)" "x"
test4 = test_squashing "? -> (? x ?)" "x"
test5 = test_splitting "? -> (? -> ?)" "x"
test6 = test_splitting "(? -> ?) -> (? -> ?)" "x"
test7 = test_squashing "(? x ?) x (? x ?)" "x"
test8 = test_squashing "(? x ?) -> (? x ?)" "x"
test9 = test_squashing_splitting "(? -> ?) -> (? -> ?)" "x"