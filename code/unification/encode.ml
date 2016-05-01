type categoryvalue = Sentence | Noun | Pronoun | Adjective | Construct | AdjP
type numbervalue = Singular | Duo | Plural
type personvalue = First | Second | Third
type gendervalue = Male | Female
type defvalue = Yes | No

let category_const = function
Sentence -> T("s",[])
  | Noun -> T("n",[])
  | Pronoun -> T("pn", [])
  | Adjective -> T("adj", [])
  | Construct -> T("cons",[])
  | AdjP -> T("ap", [])

let number_const = function
Singular -> T("sg",[])
  | Duo -> T("du", [])
  | Plural -> T("pl",[])

let person_const = function
First -> T("1",[])
  | Second -> T("2",[])
  | Third -> T("3",[])

let gender_const = function
Male -> T("m", [])
  | Female -> T("f", [])

let def_const = function
Yes -> T("yes", [])
  | No -> T("no", [])

type attributevalue = Category of categoryvalue | Number of numbervalue | Person of personvalue | Gender of gendervalue | Def of defvalue

(* indicator functions *)
let isCategory = function
Category _ -> true
  | _ -> false

let isNumber = function
Number _ -> true
  | _ -> false

let isPerson = function
Person _ -> true
  | _ -> false

let isGender = function
Gender _ -> true
  | _ -> false

let isDef = function
Def _ -> true
  | _ -> false


(* fresh variable *)
let gensym =
  let n = ref 0 in
  function () -> n := (!n)+1 ; V ("variable"^(string_of_int !n))

let term_of_value = function
Category c -> category_const c
  | Number c -> number_const c
  | Person c -> person_const c
  | Gender c -> gender_const c
  | Def c -> def_const c

let get p l =
  try
    (term_of_value
       (List.find p l) (* find a mention of the property p *)
    )
  with
    Not_found -> gensym () (* if you're unspecified, you're a variable *)

(* the encoding is
   features( category = {s,n,propn,....}
   number = {sg,du,pl}
   person = {1,2,3}
   gender = {male,female}
   def = {yes,no}
   )
*)
let encode_term l = T ("features",[ (get isCategory l);
				    (get isNumber l);
				    (get isPerson l);
                                    (get isGender l);
                                    (get isDef l);
                                  ])

exception Cannot_find_subterm of term
let subterm t n = match t with
    T ("features",l) ->
      (try List.nth l n
       with Failure "nth" -> raise (Cannot_find_subterm t))
  | _ -> raise (Cannot_find_subterm t)


let lexicon = [

  ("hua", [Category Pronoun; Number Singular; Person Third; Gender Male]);
  ("hia", [Category Pronoun; Number Singular; Person Third; Gender Female]);
  ("'i:nta", [Category Pronoun; Number Singular; Person Second; Gender Male]);
  ("'i:nti", [Category Pronoun; Number Singular; Person Second; Gender Female]);
  ("'a:na:", [Category Pronoun; Number Singular; Person First]);

  ("Sa:Hib", [Category Noun; Number Singular; Person Third; Gender Male; Def No]);
  ("aSa:Hib", [Category Noun; Number Singular; Person Third; Gender Male; Def Yes]);
  ("Sa:Hiba", [Category Noun; Number Singular; Person Third; Gender Female; Def No]);
  ("aSa:Hiba", [Category Noun; Number Singular; Person Third; Gender Female; Def Yes]);

  ("'u:sta:dh", [Category Noun; Number Singular; Person Third; Gender Male; Def No]);
  ("al'u:sta:dh", [Category Noun; Number Singular; Person Third; Gender Male; Def Yes]);
  ("'u:sta:dha", [Category Noun; Number Singular; Person Third; Gender Female; Def No]);
  ("al'u:sta:dha", [Category Noun; Number Singular; Person Third; Gender Female; Def Yes]);

  ("kwai:s", [Category Adjective; Gender Male]);
  ("kwai:sa", [Category Adjective; Gender Female]);

];;
