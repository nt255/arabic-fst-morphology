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
type root = string * string * string


let fst_of_lexicon (l : lexicon) : fst =
  let trie_of_lexicon (l : lexicon) : trie =
    let process_entry acc_t (input, output) =
      let pair_edges i o acc_l =
        let to_opt s = match s with "_" -> None | _ -> Some s in
        (to_opt i, to_opt o)::acc_l in
      enter (List.fold_right2 pair_edges (lex input) (lex output) []) acc_t in
    List.fold_left process_entry emptytrie l in
  fst_of_trie (trie_of_lexicon l)

(* very slow and convoluted *)
let lexicon_of_fst (f : fst) : lexicon =
  let rec traverse_edge (node : int) : (string * string) list =
    if IntSet.mem node f.final then ["",""] else
      let combine ((input, output) : string * string)
          (edgelist : (string * string) list) : (string * string) list =
        List.map (fun (i, o) -> input^i, output^o) edgelist in
      let check_move (origin, (input, output), result) acc =
        let to_str o = match o with Some s -> s^" " | _ -> "_ " in
        if origin = node then (result, (to_str input, to_str output))::acc else acc in
      List.fold_left (fun acc (result, edge) ->
        combine edge (traverse_edge result) @ acc)
        [] (MoveSet.fold check_move f.transitions []) in
  List.fold_left (fun acc (s0, s1) -> (String.trim s0, String.trim s1)::acc)
    [] (IntSet.fold (fun n acc -> traverse_edge n@acc) f.start [])

let format_readable (l : lexicon) : (string * string) list =
  List.fold_left (fun acc (s0, s1) ->
    ((Str.global_replace (Str.regexp " ") "" s0),
     (Str.global_replace (Str.regexp "\\( \\|_\\)*") "" s1))::acc)
    [] l

(* creates extended lexicon with roots in place *)
let extend_lexicon (l : lexicon) (rs : root list) : lexicon =
  let process_root acc (r0, r1, r2) =
    let process_entry acc_e (input, output) =
      ((Str.global_replace (Str.regexp "3") r2
          (Str.global_replace (Str.regexp "2") r1
             (Str.global_replace (Str.regexp "1") r0
                input))), output)::acc_e in
    List.fold_left process_entry acc l in
  List.fold_left process_root [] rs


(* regular sound roots *)
let arabic_roots = [
  ("k", "t", "b");
  ("f", "A", "l")
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


let arabic_fst_lexicon = fst_of_lexicon (extend_lexicon arabic_verb_forms arabic_roots)
let lexicon = format_readable (lexicon_of_fst arabic_fst_lexicon)
