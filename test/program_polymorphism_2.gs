data List a = Nil | Cons a (List a)
data Either a b = Left a | Right b
data Triple a b = Triple (List Int) (Either (Either Bool (List Bool)) (Triple Int Bool)) (List (Either a b))

left(Left x): Either a b -> a = {
  x
}

right(Right x): Either a b -> b = {
  x
}

toTriple(x, y, z, d): List Int -> Either (Either Bool (List Bool)) (Triple Int Bool) -> a -> b -> Triple a b = {
  if left(left(y))
    then Triple x y (Cons (Left z) (Cons (Right d) Nil))
    else Triple x y (Cons (Left z) (Cons (Right d) (Cons (Left z) (Cons (Right d) Nil))))
}

let x = toTriple(Cons 1 Nil, Left (Right (Cons True Nil)), 1, <[1], []>) in x