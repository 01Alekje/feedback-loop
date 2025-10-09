module Example where

thm : {P Q : Set} → ((P → Q) → P) → (P → Q) → Q
thm f pq = {!   !}

thm2 : {P Q : Set} → (P → Q) → P → Q
thm2 pq p = {!   !}




{- 

    AI COMMANDS:
        ---- PRIO 1
        GET CONTEXTS / LOAD 
        AUTO 
        GIVE 
        ---- PRIO 2
        INSERT PATTERN
        PATTERN MATCHING
        ---- PRIO 3
        HELPER FUNCTIONS

    - RESTART

-}