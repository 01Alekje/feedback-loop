exProp1 : {P Q : Set} -> (P -> P -> Q) -> ((P -> Q) -> P) -> Q
exProp1 {P} {Q} f g = f (g (λ z → f z z)) (g (λ z → f z z))

exProp2 : {P Q : Set} -> (P -> P -> Q) -> ((P -> Q) -> P) -> Q
exProp2 {P} {Q} f g = f (g (λ x → f x x)) (g (λ x → f x x))