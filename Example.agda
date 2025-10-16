module Example where


data _∨_ (A B : Set) : Set where
    left : A → A ∨ B
    right : B → A ∨ B

thm4 : {P Q : Set} → P ∨ Q → Q ∨ P
thm4 = {!   !}