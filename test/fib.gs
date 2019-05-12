data List = Nil | Cons Int List

at(l, i) : List -> Int -> Int = {
  at_(l, i, 0)
}

at_(Cons x l, i, acc) : List -> Int -> Int -> Int = {
  if acc == i 
    then x
    else at_(l, i, acc + 1)
}

fib(x) : Int -> Int = {
  if x == 0 
    then 1
    else if x == 1
      then 1
      else fib(x - 1) + fib(x - 2)
}

let fib6 = fib(6) in
  let fib5 = fib(5) in
    at(Cons fib6 (Cons fib5 Nil), 1)