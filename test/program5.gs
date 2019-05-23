data Nat = Z | S Nat
data ListNat = Nil | Cons Nat ListNat
data MaybeNat = Nothing | Just Nat

f(x) : Nat -> Nat = { x }

succ(x) : Nat -> Nat = { S x }

pred(Z) : Nat -> MaybeNat = { Nothing }
pred(S x) : Nat -> MaybeNat = { Just x }

let z = succ(Z) in z