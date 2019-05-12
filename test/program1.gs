data Pair = PairU Undirected Undirected | PairD Directed Directed

data Pair2 = Pair2U Undirected Undirected | PairDU Directed Directed
data List = Nil | Cons Pair List
data C = C

let graphA = <[1, 2, 3], [(1, 2, 0), (2, 0, 0)]> in
    let graphB = <[1, 2], [(1, 0, 0)]> in
        C