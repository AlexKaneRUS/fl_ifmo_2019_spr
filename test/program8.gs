data Nat = Z | S Nat 
data List = Nil | Cons Nat List

f(S Nil) : Nat -> Nat = { x } 

let r = f(Z) in r 