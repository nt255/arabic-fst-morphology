(* Unification by transformation                                      *)
(* from Baader and Nipkow _Term Rewriting and All That_               *)
(* section 4.7                                                        *)
type term = V of string | T of string * term list
type subst = (string * term) list

exception UNIFY of string

let rec show_term  = 
  let concat = fun x y -> if x = "" then y else if y = "" then x else x^" "^y in
  function
      T (s,[]) -> s
    | T (s,ts) -> (s^"("^(List.fold_left concat "" (List.map show_term ts))^")")
    | V v -> "?"^v

let rec variables_of_term  = 
  function
      T (s,[]) -> []
    | T (_,ts) -> List.flatten (List.map variables_of_term ts)
    | V v -> [v]

	  
          (* a substitution is a map strings naming variables to terms *)
module OrderedVariableName : (Set.OrderedType with type t=string) =
  struct
    type t = string
    let compare = compare
  end
module Substitution = Map.Make(OrderedVariableName)


let show_subst = Substitution.iter (fun variable value ->
  print_string ("?"^variable^" --> "^(show_term value)^"\n"));;


    (* is the variable x in the domain of a substitution s ? *)
let indom x s = Substitution.mem x s

    (* apply a substitution sigma to a variable x *)
exception Unknown_var of string
let app sigma x = 
  try
    Substitution.find x sigma
  with Not_found -> raise (Unknown_var ("could not find "^x^" in substitution"))

      (* lift a substitution sigma to its extension sigma-hat *)
let rec lift sigma t = match sigma,t with
  sigma, (V x) -> if (indom x sigma) then (app sigma x) else (V x)
| sigma, (T(f,ts)) -> T(f,(List.map (lift sigma) ts))

let rec occurs vn t = match vn,t with
  x, (V y) -> x=y
| x, (T(_,ts)) -> List.exists (occurs x) ts


let zip a b = try (List.combine a b)
with (Invalid_argument "List.combine") -> raise
    (UNIFY ((string_of_int (List.length a))
	    ^" versus "^
	    (string_of_int (List.length b))
	    ^" arguments"))

    (* solve: (term * term)list * subst -> subst *)
let rec solve = function
    [],s -> s
  | (V x,t)::es,s -> if (V x)=t then solve (es,s) else elim (x,t,es,s)
  | (t,V x)::es,s -> elim (x,t,es,s)
  | ((T (f,ts),T (g,us))::es,s) ->
      if f=g then (solve ((zip ts us)@es,s))
      else raise (UNIFY (f^" conflicts with "^g))
and
    (* elim: vname * term * (term * term) list * subst -> subst *)
    elim (x,t,es,s) =
  if (occurs x t) then raise (UNIFY (x^" occurs in "^(show_term t))) else
  let xt = (lift (Substitution.add x t (Substitution.empty))) in
  solve
    (List.map (function (t1,t2) -> ((xt t1),(xt t2))) es, (* apply xt to old S *)
     (Substitution.add x t (Substitution.mapi (fun y u  -> (xt u)) s)))
