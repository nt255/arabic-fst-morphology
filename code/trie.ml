(*
  store a lexicon as a Trie

  examples for September 17th 2013

  make sure to #use "utilities.ml" first
  to get functions such as explode, implode, time
*)

(* 11/21 adapted trie to contain fst edges *)


type trie = Trie of (bool * arcs)
and arcs = ((string option * string option) * trie) list

let emptytrie = Trie (false,[])


(* Okasaki-style *)
let rec enter (w : (string option * string option) list) (t : trie) : trie = match (w,t) with
     [],Trie(_,l) -> Trie(true,l)
| k::ks,Trie(b,l) ->
    let subtrie =
      try
	List.assoc k l (* Q. do we already have an arc labeled 'k' ? *)
      with Not_found -> emptytrie (* A. No, make up some default new one *)
    in
    let tprime = enter ks subtrie in
      Trie(b,(k,tprime)::(List.remove_assoc k l))
