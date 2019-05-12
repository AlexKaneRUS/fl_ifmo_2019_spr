data Pair = PairU Undirected Undirected | PairD Directed Directed

func(PairD graphA graphB, PairU graphC graphD, x) : Pair -> Pair -> Int -> Bool = {
    let aaa = x + x in
      let bbb = graphA * graphB in
        if aaa > 4 
          then bbb == graphA
          else graphC / graphD == graphC
}

let graphA = <<[1, 2, 3], [(1, 2, 0), (2, 0, 0)]>> in
    let graphB = <<[1, 2], [(1, 0, 0)]>> in
        func(PairD <[1], []> <[2], []>, PairU graphA graphB, 0)