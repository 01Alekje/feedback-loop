module Proof where

open import Base

thm : (n : Nat) → n + zero ≡ n
thm = {!   !}

thm1 : (n m : Nat) → suc (n + m) ≡ n + suc m
thm1 = {!   !}

thm2 : (n m : Nat) → n + m ≡ m + n
thm2 = {!   !}

thm3 : {A B : Set} → (f : A → B) → (xs ys : List A) → map f (xs ++ ys) ≡ map f xs ++ map f ys 
thm3 = {!   !}

thm4 : {A B : Set} → (f : A → B → B) → (b : B) → (xs ys : List A) → foldr f b (xs ++ ys) ≡ foldr f (foldr f b ys) xs
thm4 = {!   !}
