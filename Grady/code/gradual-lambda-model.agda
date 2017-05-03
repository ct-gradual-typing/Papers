module gradual-lambda-model where

open import nat

data Object : Set where
  Atom   : ℕ → Object
  Unit   : Object
  Unknown : Object
  _=>_   : Object → Object → Object
  _x_    : Object → Object → Object

data _===>_ : Object → Object → Set where
  splitA  : Unknown ===> (Unknown => Unknown)
  squashA : (Unknown => Unknown) ===> Unknown
  splitP  : Unknown ===> (Unknown x Unknown)
  squashP : (Unknown x Unknown) ===> Unknown
  boxT    : ∀(A : ℕ) → (Atom A) ===> Unknown
  unboxT  : ∀(A : ℕ) → Unknown ===> (Atom A)
  boxU    : Unit ===> Unknown
  unboxU  : Unknown ===> Unit
  proj1   : ∀{A B} → (A x B) ===> A
  proj2   : ∀{A B} → (A x B) ===> B
  _xM_    : ∀{A B C D} → (f : A ===> C) → (g : B ===> D) → (A x B) ===> (C x D)
  ⟨_,_⟩   : ∀{A B C} → (f : A ===> B) → (g : A ===> C) → A ===> (B x C)
  ◇       : (A : Object) → Unit ===> A
  curry   : ∀{A B C} → (f : (A x B) ===> C) → A ===> (B => C)
  uncurry : ∀{A B C} → (f : A ===> (B => C)) → (A x B) ===> C
  app     : ∀{A B} → ((A => B) x A) ===> B
  _∘_     : ∀{A B C} → (f : A ===> B) → (g : B ===> C) → A ===> C
  id      : (A : Object) → A ===> A

infix  31 ⟨_,_⟩ _xM_
infixl 30 _∘_

data _≡M_ : ∀{A B} → (A ===> B) → (A ===> B) → Set where
  id₁      : ∀{A B}{f : A ===> B} → f ∘ (id B) ≡M f
  id₂      : ∀{A B}{f : A ===> B} → ((id A) ∘ f) ≡M f
  assoc    : ∀{A B C D}{f : A ===> B}{g : B ===> C}{h : C ===> D} → (f ∘ g) ∘ h ≡M f ∘ (g ∘ h)
  cart₁    : ∀{A B C}{f : C ===> A}{g : C ===> B} → ⟨ f , g ⟩ ∘ proj1 ≡M f
  cart₂    : ∀{A B C}{f : C ===> A}{g : C ===> B} → ⟨ f , g ⟩ ∘ proj2 ≡M g
  cart₃    : ∀{A B C}{f : C ===> A}{g : C ===> B}{h : C ===> (A x B)} → (h ∘ proj1) ≡M f → (h ∘ proj2) ≡M g → h ≡M ⟨ f , g ⟩
  term     : ∀{A}{f : Unit ===> A} → f ≡M (◇ A)
  exp₁     : ∀{A B C}{f : (A x B) ===> C} → uncurry (curry f) ≡M f
  exp₂     : ∀{A B C}{f : A ===> (B => C)} → curry (uncurry f) ≡M f
  exp₃     : ∀{A B C}{f : (A x B) ===> C} → (((curry f) xM (id B)) ∘ app) ≡M f
  retractA : (squashA ∘ splitA) ≡M (id (Unknown => Unknown))
  retractP : (squashP ∘ splitP) ≡M (id (Unknown x Unknown))
  retractT : (A : ℕ) → ((boxT A) ∘ (unboxT A)) ≡M (id (Atom A))
  retractU : (boxU ∘ unboxU) ≡M (id Unit)
  refl     : ∀{A B}{f : A ===> B} → f ≡M f
  sym      : ∀{A B}{f g : A ===> B} → f ≡M g → g ≡M f
  trans    : ∀{A B}{f g h : A ===> B} → f ≡M g → g ≡M h → f ≡M h

infix  4 _MorEq_
infix  3 _QED
infixr 2 _≡M⟨_⟩_
infix  1 proof_

data _MorEq_ {A B}(x y : A ===> B) : Set where
  meq : (pf : x ≡M y) → x MorEq y

proof_ : ∀{A B}{f g : A ===> B} → f MorEq g → f ≡M g
proof (meq p) = p

_≡M⟨_⟩_ : ∀{A B} → (f : A ===> B){g h : A ===> B} → f ≡M g → g MorEq h → f MorEq h
f ≡M⟨ p₁ ⟩ (meq p₂) = meq (trans p₁ p₂)

_QED : ∀{A B}(f : A ===> B) → f MorEq f
f QED = meq refl

cart₄ : ∀{A B} → ⟨ proj1 , proj2 ⟩ ≡M id (A x B)
cart₄ {A}{B} = proof
                 ⟨ proj1 , proj2 ⟩ ≡M⟨ sym (cart₃ {A}{B}{(A x B)} id₂ id₂) ⟩ id (A x B)
               QED

