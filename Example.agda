module Example where 


data _∨_ (A B : Set) : Set where
    left : A → A ∨ B
    right : B → A ∨ B

data _∧_ (A B : Set) : Set where
    both : A → B → A ∧ B

data ⊥ : Set where

¬ : Set → Set
¬ A = A → ⊥

any : {A : Set} → ⊥ → A
any ()
    
thm1 : {A B : Set} → (¬ A ∨ B) → (A → B)
thm1 (left x) a = any (x a)
thm1 (right x) a = x

thm2 : {A B : Set} → (¬ A ∧ ¬ B) → ¬ (A ∨ B)
thm2 (both x₁ x₂) (left x) = x₁ x
thm2 (both x₁ x₂) (right x) = x₂ x

thm3 : {A B C : Set} → (B ∧ A → C) → (A ∧ B → C)
thm3 f (both x x₁) = f (both x₁ x)


thm4 : ⊥
thm4 = thm4