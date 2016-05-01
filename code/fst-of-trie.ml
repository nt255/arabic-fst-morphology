(* 11/21 adapted to convert fst-edge tries to fst *)


let rec fst_of_node parentnumber = function
Trie(false,arcs) -> List.fold_left (fst_of_arc parentnumber) (parentnumber,{emptyfst with states=(IntSet.singleton parentnumber)}) arcs
  | Trie(true,arcs) -> let s = (IntSet.singleton parentnumber) in
                       List.fold_left (fst_of_arc parentnumber) (parentnumber,{emptyfst with states=s; final=s}) arcs
and fst_of_arc parentnumber (siblingnumber,m0) = function
(edge, t) ->
  let (childnumber,m) = fst_of_node (siblingnumber+1) t in
  (childnumber,
   { states = IntSet.union m0.states m.states;
     transitions = MoveSet.add (parentnumber, edge,(siblingnumber+1)) (MoveSet.union m0.transitions m.transitions);
     start = IntSet.union m0.start m.start;
     final = IntSet.union m0.final m.final}
  )

let fst_of_trie = function
Trie(b,l) -> Pervasives.snd
  (List.fold_left (fst_of_arc 0)
     (0,{emptyfst with states = IntSet.singleton 0; start=IntSet.singleton 0; final = if b then IntSet.singleton 0 else IntSet.empty}) l)
