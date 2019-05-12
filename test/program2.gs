data Pair = PairU Undirected Undirected | PairD Directed Directed
data Graph = D Directed | U Undirected

min(PairU graphA graphB) : Pair -> Graph = {
    if graphA < graphB 
      then U graphA
      else U graphB
}
min(PairD graphA graphB) : Pair -> Graph = {
    if graphA < graphB 
      then D graphA
      else D graphB
}

let graphA = <<[1, 2, 3], [(1, 2, 0), (2, 0, 0)]>> in
    let graphB = <<[1, 2], [(1, 0, 0)]>> in
        min(PairU graphA graphB)