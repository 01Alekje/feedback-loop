module Example where

thm : {P Q : Set} → ((P → Q) → P) → (P → Q) → (P ∧ Q) → Q
thm = ?