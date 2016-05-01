(*  draw Attrbute-Value matrices

  output with Qtree  http://www.ctan.org/pkg/qtree
   and  avm          http://nlp.stanford.edu/cmanning/tex/

assuming Unification.term trees
and a term encoding of attribute-value matrices that looks exactly like this

   the encoding is
   features( category = {s,n,propn,....}
   number = {sg,du,pl}
   person = {1,2,3}
   gender = {male,female}
   def = {yes,no}

*)


let rec avm_of = function
    V varname -> "\\@{"^varname^"} \\ \n "
  | T (name,[]) -> name^" \\\\ \n "
  | info ->
      ("{\\begin{avm} \\["^
	 (specified info "category")^
	 (specified info "number")^
	 (specified info "person")^
	 (specified info "gender")^
	 (specified info "def")^
       "\\] \\end{avm}}")
and
  specified t f =
  let isconst = function  (* leave out variables *)
        T (_,[]) -> true
       | _ -> false  in
  let format name value =
     (name^" & "^(avm_of value)) in
 match f with
  "category" -> let x = (subterm t) 0 in  if isconst x then (format f x) else ""
| "number" -> let x = (subterm t) 1 in  if isconst x then (format f x) else ""
| "person" -> let x = (subterm t) 2 in  if isconst x then (format f x) else ""
| "gender" -> let x = (subterm t) 3 in  if isconst x then (format f x) else ""
| "def" -> let x = (subterm t) 4 in  if isconst x then (format f x) else ""
| _ -> ""


  let qtree_of_tree t =
    let rec qtAux recursionLevel tree =
      if recursionLevel > 20 then failwith "your tree is too deep for qtree v3.1"
      else match tree with
	  Leaf (w,fs) -> " "^w^" "
	| Node (fs,dtrs) ->
	      if (List.length dtrs > 5)
	      then failwith "your tree is too wide for qtree v3.1"
	      else "[.{"^(avm_of fs)^"}"^(List.fold_left (^) " " (List.map (qtAux (recursionLevel+1)) dtrs))^" ] "
    in
      qtAux 0 t

  let latex_of tree = ("\\documentclass{article}\n\\usepackage{avm}\\usepackage{qtree}\n\\pagestyle{empty}\n\\begin{document}"^
		   "\n \\Tree "^(qtree_of_tree tree)^"\n"^
		   "\\end{document} \n")
