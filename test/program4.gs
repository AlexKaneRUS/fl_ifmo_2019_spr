data Pair = PairU Undirected Undirected | PairD Directed Directed
data Triple = T Undirected Directed Bool

func(p1, PairD graphA graphB) : Pair -> Pair -> Bool = {
    if (let a = mkDirected() in graphA > a)
      then True
      else 
        let PairU graphC graphD = p1 in
          if func(T graphC a False) 
            then False
            else 1 > 1
}

func(t) : Triple -> Bool = {
    let T a b c = t in
      let x = 2 ^ 2 in
        let a = b in
          a == b
}

mkDirected() : Directed = {
    <[1], []>
}

func(PairU <<[1], []>> <<[2], []>>, PairD mkDirected() mkDirected())