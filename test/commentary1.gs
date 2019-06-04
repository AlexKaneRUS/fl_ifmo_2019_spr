data Nat = Z | S Nat
{-
  hey, guys
-}
data ListNat = Nil | Cons Nat ListNat
-- ooooo
data MaybeNat = Nothing | Just Nat

f(x) : Nat -> Nat = { x }

-- succ function is pretty good
-- I love it
succ(x) : Nat -> Nat = { S {- really, even, here? {- for sure -}  hmm -} x }
-- and...

{-
  hello, this is pred function
  it is very very cool
  {-
    please use it
  -}
    
  as much as you want
-}
pred(Z) : Nat -> MaybeNat = { Nothing }
pred(S x) : Nat -> MaybeNat = { 
   if 2 > 6 -- this condition is never satisfied
     then Nothing -- no, no, nothing here
     else Just x -- but here...
  }

-- aaaa
let z = succ(Z) in z + {- look, commentary here -} 3
-- This is us



