module Example where

open import Data.Nat

data Nat : Set where
    zero : Nat
    suc : Nat → Nat

data _∧_ (A B : Set) : Set where
    both : A → B → A ∧ B

data _∨_ (A B : Set) : Set where
    inl : A → A ∨ B
    inr : B → A ∨ B

thm : Nat → Nat → Nat
thm zero blah = ?
thm (suc n) blah = ?
    where 
        f : {Q P : Set} → P → Q
        f p = {!   !}

        g : {Q P : Set} → Q → P
        g f = {!   !}
