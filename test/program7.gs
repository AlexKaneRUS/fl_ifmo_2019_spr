data Nat = Z | S Nat
data ListNat = Nil | Cons Nat ListNat
data NtN = NatToNat (Nat -> Nat) 

succ(x) : Nat -> Nat = { S x }

map(f, Nil) : NtN -> ListNat -> ListNat = { Nil }
map(NatToNat f, Cons hd tl) : NtN -> ListNat -> ListNat = { Cons (f(hd)) (map(NatToNat f, tl)) }

let zero = Z in 
let one = S zero in 
let two = succ(one) in 
let lst = Cons zero (Cons one (Cons two Nil)) in 
map(NatToNat succ, lst)