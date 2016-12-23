module Boxing where

import Prelude hiding (id)

import Syntax
import Parser
import Pretty
import CatTools
    
-- Converts a type into its skeleton.

skeleton_of :: Type -> Type
skeleton_of b | is_atomic b = U
skeleton_of (Arr a b)  = Arr (skeleton_of a) (skeleton_of b)
skeleton_of (Prod a b) = Prod (skeleton_of a) (skeleton_of b)

is_skeleton :: Type -> Bool
is_skeleton U = True
is_skeleton (Arr a b) = (is_skeleton a) && (is_skeleton b)
is_skeleton (Prod a b) = (is_skeleton a) && (is_skeleton b)
is_skeleton _ = False

-- Casting morphisms that convert types to their skeletons.

lifted_box :: Type -> LFreshM Term

lifted_box U = id U

lifted_box (Arr a1 a2) = do
  c1 <- lifted_unbox a1
  c2 <- lifted_box a2
  arrow_func a1 a2 (skeleton_of a1) c1 c2

lifted_box (Prod a1 a2) = do
  c1 <- lifted_box a1
  c2 <- lifted_box a2
  prod_func a1 a2 c1 c2

lifted_box a = return $ Box a

lifted_unbox :: Type -> LFreshM Term

lifted_unbox U = id U

lifted_unbox (Arr a1 a2)  = do
  c1 <- lifted_box a1
  c2 <- lifted_unbox a2
  arrow_func (skeleton_of a1) (skeleton_of a2) a1 c1 c2

lifted_unbox (Prod a1 a2) = do
  c1 <- lifted_unbox a1
  c2 <- lifted_unbox a2
  prod_func (skeleton_of a1) (skeleton_of a2) c1 c2
        
lifted_unbox a = return $ Unbox a    

test_lifted_box :: String -> String
test_lifted_box sty = case q of
                        Right ty -> let b = runLFreshM $ lifted_box ty
                                     in runLFreshM $ prettyTerm b
                        Left m -> m
 where
   q = parseType sty

test_lifted_unbox :: String -> String
test_lifted_unbox sty = case q of
                        Right ty -> let b = runLFreshM $ lifted_unbox ty
                                     in runLFreshM $ prettyTerm b
                        Left m -> m
 where
   q = parseType sty

-- lifted_split : Builds a term from U to the input type.  This
-- function assumes that the input type is a skeleton.

lifted_split :: Type -> LFreshM Term

lifted_split U = id U
         
lifted_split (Arr U U) = return $ Split -- Needs type annotation

lifted_split (Arr s1 s2) = do
  t1 <- lifted_squash s1
  t2 <- lifted_split s2
  t3 <- arrow_func U U s1 t1 t2
  comp U Split t3 -- Needs type annotation

lifted_split (Prod U U) = return $ Split -- Needs type annotation

lifted_split (Prod s1 s2) = do
  t1 <- lifted_split s1
  t2 <- lifted_split s2
  t3 <- prod_func U U t1 t2
  comp U Split t3 -- Needs type annotation

lifted_split a = error "lifted_split cannot be applied to a non-skeleton type."

-- lifted_squash : Builds a term from the input type to U.  This
-- function assumes that the input type is a skeleton.

lifted_squash :: Type -> LFreshM Term

lifted_squash U = id U

lifted_squash (Arr U U) = return $ Squash -- Needs type annotation

lifted_squash (Arr s1 s2) = do
  t1 <- lifted_split s1
  t2 <- lifted_squash s2
  t3 <- arrow_func s1 s2 U t1 t2
  comp (Arr s1 s2) t3 Squash -- Needs type annotation

lifted_squash (Prod U U) = return $ Squash -- Needs type annotation

lifted_squash (Prod s1 s2) = do
  t1 <- lifted_squash s1
  t2 <- lifted_squash s2
  t3 <- prod_func s1 s2 t1 t2
  comp (Prod s1 s2) t3 Squash -- Needs type annotation

lifted_squash a = error "lifted_squash cannot be applied to a non-skeleton type."

-- General box and unbox.

box :: Type -> LFreshM Term
box a = do
  s <- lifted_box a
  t <- lifted_squash (skeleton_of a)
  comp a s t

unbox :: Type -> LFreshM Term
unbox a = do  
  s <- lifted_split (skeleton_of a)
  t <- lifted_unbox a
  comp a s t

