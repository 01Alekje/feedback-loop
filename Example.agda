module Example where


thm1 : {P Q : Set} → (P → (P → Q)) → ((P → Q) → P) → Q
thm1 = {!   !}

thm2 : {P Q : Set} → (((P → Q) → Q) → Q) → P → Q
thm2 = {!   !}

thm3 : {P Q : Set} → ((P → Q) → P) → (P → Q) → Q
thm3 = {!   !}


