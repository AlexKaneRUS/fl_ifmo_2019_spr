data Nat = Z | S Nat
data ListNat = Nil | Cons Nat ListNat
data MaybeNat = Nothing | Just Nat

succ(x) : Nat -> Nat = { S x }

lt(Z, Z) : Nat -> Nat -> Bool = { False }
lt(Z, S x) : Nat -> Nat -> Bool = { True }
lt(S x, S y) : Nat -> Nat -> Bool = { lt(x, y) }

makeList(x, y) : Nat -> Nat -> ListNat = {
  if lt(x, y) 
  then Cons x (makeList(succ(x), y))
  else Nil
}

let zero = Z in 
let one = S zero in 
let two = succ(one) in 
makeList(zero, two)