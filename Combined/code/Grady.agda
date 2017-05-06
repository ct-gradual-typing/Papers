module Grady where

open import list
open import nat

Name : Set
Name = ℕ

data GType : Set where
  Unit : GType
  Nat : GType
  Unknown : GType
  Imp : GType → GType → GType
  Prod : GType → GType → GType  

data GTerm : Set where
  fvar    : Name → GTerm
  bvar    : Name → GTerm
  splitI  : GTerm
  splitP  : GTerm
  squashI : GTerm
  squashP : GTerm
  box     : GType → GTerm
  unbox   : GType → GTerm
  error   : GType → GTerm
  lam     : GType → GTerm
  app     : GTerm → GTerm → GTerm
  pair    : GTerm → GTerm → GTerm
  fst     : GTerm → GTerm
  snd     : GTerm → GTerm
  zero    : GTerm → GTerm
  case    : GTerm → GTerm → GTerm
  


