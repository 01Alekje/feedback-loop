--Base file containg some basic theorems and definitions.
module Base where
    
infix 4 _≡_
infixr 6 _::_
infix 6 _∧_
infix 5 _∨_
infixr 6 _++_
infixl 5 _+_
infixl 6 _*_

data _≡_ {A : Set} : A → A → Set where
    refl : {a : A} → a ≡ a

sym : {A : Set} → {a b : A} → a ≡ b → b ≡ a
sym refl = refl

trans : {A : Set} → {a b c : A} → a ≡ b → b ≡ c → a ≡ c
trans refl refl = refl

cong : {A B : Set} → {x y : A} → (F : A → B) → x ≡ y → F x ≡ F y
cong _ refl = refl

data _∧_ (A B : Set) : Set where
    both : A → B → A ∧ B

data _∨_ (A B : Set) : Set where
    left : A → A ∨ B
    right : B → A ∨ B

data ⊥ : Set where 

¬_ : Set → Set
¬ A = A → ⊥ 

any : {A : Set} → ⊥ → A
any ()

data Nat : Set where
    zero : Nat
    suc : Nat → Nat

_+_ : Nat → Nat → Nat
zero + m = m
suc n + m = suc (n + m)

_*_ : Nat → Nat → Nat
zero * m = zero
suc n * m = m + (n * m)

data List (A : Set) : Set where
    [] : List A
    _::_ : A → List A → List A
    
_++_ : {A : Set} → List A → List A → List A
[] ++ ys = ys
(x :: xs) ++ ys = x :: (xs ++ ys)

map : {A B : Set} → (A → B) → List A → List B
map f [] = []
map f (x :: xs) = f x :: map f xs

foldr : {A B : Set} → (A → B → B) → B → List A → B
foldr f z [] = z
foldr f z (x :: xs) = f x (foldr f z xs)

data Σ (A : Set) (B : A → Set) : Set where
  _,_ : (x : A) → B x → Σ A B
