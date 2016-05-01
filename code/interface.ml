#load "str.cma";;

#use "utilities.ml";;
#use "trie.ml";;
#use "fst.ml";;
#use "fst-of-trie.ml";;
#use "visualizeFST.ml";;


(* space separated string of inputs followed
   by space separated string of outputs *)
type entry = string * string
type lexicon = entry list


let fst_of_lexicon (l : lexicon) : fst =
  let trie_of_lexicon (l : lexicon) : trie =
    let process_entry acc_t (input, output) =
      let pair_edges i o acc_l =
        let to_opt s = match s with "_" -> None | _ -> Some s in
        (to_opt i, to_opt o)::acc_l in
      enter (List.fold_right2 pair_edges (lex input) (lex output) []) acc_t in
    List.fold_left process_entry emptytrie l in
  fst_of_trie (trie_of_lexicon l)

let fst_of_roots (l : string list) : fst =
  let ret = fst_of_lexicon (List.map (fun r -> ("1 2 3", r)) l) in
  {ret with transitions =
      MoveSet.fold (fun (s, _, r) acc -> MoveSet.add (r, (None, None), s) acc)
        ret.transitions ret.transitions}

let recognize_from_form_and_root (l : fst) (r : fst) (w : string list) : string option =
  let traverse_fst_form (form_nodes : IntSet.t) (root_nodes : IntSet.t) (output : string)
      (c : string) : IntSet.t * IntSet.t * string =
    let traverse_fst_root (root_nodes : IntSet.t) (c : string) (cn : string option) : IntSet.t =
      let ret = IntSet.fold (fun n acc ->
        MoveSet.fold (fun (s, (i, o), r) acc_m ->
          if s = n && i = cn && o = Some c then IntSet.add r acc_m else acc_m)
          r.transitions acc) root_nodes IntSet.empty in
      IntSet.fold (fun n acc ->
        MoveSet.fold (fun (s, (i, o), r) acc_m ->
          if s = n && i = None && o = None then IntSet.add r acc_m else acc_m)
          r.transitions acc) ret ret in
    IntSet.fold (fun n acc ->
      MoveSet.fold (fun (s, (i, o), r) (is, nodes, str) ->
        let to_str o = match o with Some s -> s^" " | _ -> "_ " in
        match i with Some "1" | Some "2" | Some "3" ->
          let next_nodes = traverse_fst_root nodes c i in
          if s = n && next_nodes != IntSet.empty
          then (IntSet.add r is, next_nodes, str^c^" "^(to_str o)) else (is, nodes, str)
        | _ -> if s = n && i = Some c
          then (IntSet.add r is, nodes, str^(to_str o)) else (is, nodes, str))
        l.transitions acc) form_nodes (IntSet.empty, root_nodes, output) in
  let (form_final, root_final, output) = List.fold_left
    (fun (form, root, output) c -> traverse_fst_form form root output c)
    (l.start, r.start, "") w in
  let format (s : string) : string =
    let features, consonants = List.fold_left (fun (f, r) t ->  match t with
        "P" | "M" | "I" | "II" | "III" | "IV"
      | "V" | "VI" | "VII" | "VIII" | "IX" | "X" -> (f^t, r)
      | _ -> (f, if List.mem t r then r else t::r)) ("", [])
      (lex (Str.global_replace (Str.regexp "_") "" s)) in
    (List.fold_left (fun acc c -> c^acc) "" consonants)^" "^features in
  if not (IntSet.is_empty (IntSet.inter form_final l.final))
    && not (IntSet.is_empty (IntSet.inter root_final r.final)) then
    Some (format output) else None


(* regular sound roots *)
let arabic_roots = [
  "k t b";
  "f A l";
  "k m l"
]

(* 1,2 and 3 indicate each of the three
   consonants of the root respectively *)
let arabic_verb_forms = [
  ("1 a 2 a 3 a", "P _ _ I _ _");
  ("1 a 2 2 a 3 a", "P _ _ II _ _ _");
  ("1 a: 2 a 3 a", "P III _ _ _ _");
  ("'a 1 2 a 3 a", "P IV _ _ _ _");
  ("ta 1 a 2 2 a 3 a", "P _ V _ _ _ _ _");
  ("ta 1 a: 2 a 3 a", "P _ VI _ _ _ _");
  ("in 1 a 2 a 3 a", "P VII _ _ _ _ _");
  ("i 1 ta 2 a 3 a", "P _ VIII _ _ _ _");
  ("i 1 2 a 3 3 a", "P _ IX _ _ _ _");
  ("ista 1 2 a 3 a", "P X _ _ _ _");
  ("ya 1 2 u 3 u", "M _ _ I _ _");
  ("yu 1 a 2 2 i 3 u", "M _ II _ _ _ _ _");
  ("yu 1 a: 2 i 3 u", "M _ III _ _ _ _");
  ("yu 1 2 i 3 u", "M _ IV _ _ _");
  ("yata 1 a 2 2 a 3 u", "M _ V _ _ _ _ _");
  ("yata 1 a: 2 a 3 u", "M _ VI _ _ _ _");
  ("yan 1 a 2 i 3 u", "M VII _ _ _ _ _");
  ("ya 1 ta 2 i 3 u", "M _ VIII _ _ _ _");
  ("ya 1 2 a 3 3 u", "M _ _ IX _ _ _");
  ("yasta 1 2 i 3 u", "M X _ _ _ _")
]


let recognize (s : string) : string option = recognize_from_form_and_root
  (fst_of_lexicon arabic_verb_forms) (fst_of_roots arabic_roots) (lex s)
