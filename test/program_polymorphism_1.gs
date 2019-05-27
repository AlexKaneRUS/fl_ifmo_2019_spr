data List a = Nil | Cons a (List a)

head(Cons a l) : List a -> a = {
  a
}

let l = Cons 1 (Cons 2 Nil) in
  head(l)