data Pair = PairU Undirected Undirected | PairD Directed Directed

min(PairU graphA graphB) : Pair -> Undirected = {
    if graphA < graphB 
      then graphA
      else graphB
}
min(PairD graphA graphB) : Pair -> Directed = {
    if graphA < graphB 
      then graphA
      else graphB
}

let graphA = <<[1, 2, 3], [(1, 2, 0), (2, 0, 0)]>> in
    let graphB = <<[1, 2], [(1, 0, 0)]>> in
        min(PairU graphA graphB)