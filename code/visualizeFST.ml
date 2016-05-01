(* visualizing automata using GraphViz                  *)
(* from João Saraiva's Lecture Notes entitled           *)
(*      Language Processing (with a functional flavour) *)
(* http://www.di.uminho.pt/~jas/                        *)

(* 11/21 adapted to convert fst-edge tries to fst *)


(* this function puts list elements in a string, one per line *)
let oneperline l =
  let between x y =
    if x = "" then y (* first elt of l *)
    else
      (if y = "" then x^"\n" else x^"\n"^y) in
  List.fold_right between l ""


let dot_of_start mach =
  List.map
    (function z ->
      "\""^(string_of_int z)^"\""
      ^" [shape=doublecircle , color=green];" )
  (IntSet.elements mach.start)

let dot_of_final mach =
  List.map
    (function z ->
      "\""^(string_of_int z)^"\""
      ^" [shape=doublecircle , color=red];" )
  (IntSet.elements mach.final)


let dot_of_move = function (origin, (input, output), result) ->
  let s_of_opt opt = match opt with Some s -> s | None -> "epsilon" in
  let edge_label = (s_of_opt input)^" : "^(s_of_opt output) in
    "\""^(string_of_int origin)^"\""
    ^" -> "^"\""^(string_of_int result)^"\""
    ^" [label = \""^edge_label^"\"];"


let dot_of_moves mach = List.map dot_of_move (MoveSet.elements mach.transitions)

let dot_of_fst mach name =
  "digraph "^name^" {\n"
            ^"rankdir = LR ;\n"
            ^(oneperline (dot_of_start mach))
            ^(oneperline (dot_of_final mach))
            ^"node [shape=circle , color=black];\n"
            ^(oneperline (dot_of_moves mach))
            ^"}"


let write_fst mach name =
  let oc = open_out (name^".dot") in
  output_string oc (dot_of_fst mach name);
  close_out oc;;
