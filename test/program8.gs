data Nat = Z | S Nat 
data List = Nil | Cons Nat List

f(Cons a (Cons x (Cons y Nil)), S (S Z)) : List -> Nat -> Nat = { 
  y
} 

let S (S (S x)) = f(Cons Z (Cons (S Z) (Cons Z Nil)), Z) in
  f(Cons (S (S (S (S Z)))) (Cons (S Z) (Cons Z Nil)), x)