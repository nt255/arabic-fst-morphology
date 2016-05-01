(* parser that uses feature structures
   coded by John Hale
   simplifying van Eijck and Unger 2010 section 9.4
   compare Francez and Wintner 2012, Chapter 5


   append_unique is now in utilities.ml
   encode.ml has the lexicon
   unifier from Baader and Nipkow is in unification.ml

   make sure to read in, in this order

   #use "unification.ml";;
   #use "utilities.ml";;
   #use "encode.ml";;
   then this file
   #use "qtree.ml";;

*)


(* familiar notion of parse tree
   leaves have a "real" word
*)
type 'a tree = Leaf of  string * 'a | Node of 'a * 'a tree list

let label = function
Leaf (_,x) -> x
  | Node (x,_) -> x

let daughters = function
Leaf _ -> []
  | Node (_,dtrs) -> dtrs

(* types for parsers *)
type suffix = string list

(* multipath_parser implements nondeterminism via a polymorphic list of results *)
type 'a answer = 'a * suffix
and 'a multipath_parser = suffix -> ('a answer) list


let (seq : 'a multipath_parser -> 'b multipath_parser -> ('a * 'b) multipath_parser) =
  fun parser1 parser2 input ->
    List.flatten
      (List.map
	 (function (res1,suf1) -> (* for every p1 result... *)
           List.map (function (res2,suf2) -> (res1,res2),suf2) (* and pair up the results *)
	     (parser2 suf1) (* ...apply p2 *)
	 )
         (parser1 input)
      )

let (alt : 'a multipath_parser -> 'a multipath_parser -> 'a multipath_parser) =
  fun parser1 parser2 input ->
    append_unique (parser1 input) (parser2 input)


(* allows unification failures to filter the answers *)
let (gives : 'a multipath_parser -> ('a -> 'b) -> 'b multipath_parser) =
  fun parser1 treatment input ->
    List.fold_left
      (fun prev (result,suffix) ->
	(try
	   (treatment result,suffix)::prev
	 with UNIFY _ -> prev)) [] (parser1 input)

(* consider the word we see before us, word
   find lexical entries that
   a. are spelled the same as "word"
   b. have the desired category specification

   return as many of them as there are matches. encode their feature structures as a term,
   so that they may be used in future unifications
*)
let (terminal : categoryvalue -> term tree multipath_parser) =
  fun desiredcat position -> match position with
    word::remainder ->
      List.map (function entry -> (Leaf ((fst entry),(encode_term (snd entry))),remainder))
	(List.filter (function (phon,feat) -> (phon = word) && (List.mem (Category desiredcat) feat)) lexicon)
  | [] -> []

let ( &. ) = seq
let ( |. ) = alt
let ( >. ) = gives


(* the encoding is
   features( category = {s,n,propn,....}
   number = {sg,du,pl}
   person = {1,2,3}
   gender = {male,female}
   def = {yes,no}
   )
*)

let unifyNumberPersonGender (leftdtr,rightdtr) =
  let fs1 = T ("features",[(subterm (label leftdtr) 1); (* number *)
			   (subterm (label leftdtr) 2); (* person *)
                           (subterm (label leftdtr) 3)  (* gender *)
			  ]) in
  let fs2 = T ("features",[(subterm (label rightdtr) 1); (* number *)
			   (subterm (label rightdtr) 2); (* person *)
                           (subterm (label rightdtr) 3)  (* gender *)
			  ]) in
  let varterm = T ("features",[V "Number";V "Person";V "Gender"]) in
  let st = solve ([(varterm,fs1);(varterm,fs2)],Substitution.empty) in
  let newlabel = (lift st) (label leftdtr) in (* arbitrary *)
  Node (newlabel,[leftdtr;rightdtr])

let unifyGenderDef (leftdtr,rightdtr) =
  let fs1 = T ("features",[(subterm (label leftdtr) 3); (* gender *)
			   (subterm (label leftdtr) 4)  (* def    *)
			  ]) in
  let fs2 = T ("features",[(subterm (label rightdtr) 3); (* gender *)
			   (subterm (label rightdtr) 4)  (* def    *)
			  ]) in
  let varterm = T ("features",[V "Gender";V "Def"]) in
  let st = solve ([(varterm,fs1);(varterm,fs2)],Substitution.empty) in
  let newlabel = (lift st) (label leftdtr) in (* arbitrary *)
  Node (newlabel,[leftdtr;rightdtr])

let unifyGender (leftdtr,rightdtr) =
  let fs1 = T ("features",[(subterm (label leftdtr) 3)]) in
  let fs2 = T ("features",[(subterm (label rightdtr) 3)]) in
  let varterm = T ("features",[V "Gender"]) in
  let st = solve ([(varterm,fs1);(varterm,fs2)],Substitution.empty) in
  let newlabel = (lift st) (label leftdtr) in (* arbitrary *)
  Node (newlabel,[leftdtr;rightdtr])

let requireDef def tree =
  let st = solve ([(def_const def),(subterm (label tree) 4)],Substitution.empty) in (* will raise exception if un-unifiable *)
  let newfs =  (lift st) (label tree) in
  match tree with
    Leaf (w,_) -> Leaf (w,newfs)
  | Node (_,dtrs) -> Node (newfs,dtrs)

let assignCategory cat tree =
  let newfs = T ("features",[(category_const cat);         (* category *)
			     (subterm (label tree) 1);     (* number   *)
			     (subterm (label tree) 2);     (* person   *)
			     (subterm (label tree) 3);     (* gender   *)
			     (subterm (label tree) 4)      (* def      *)
			    ]) in
  match tree with
    Leaf (w,_) -> Leaf (w,newfs)
  | Node (_,dtrs) -> Node (newfs,dtrs)


let projectLeft (leftdtr,rightdtr) =  Node ((label leftdtr),[leftdtr;rightdtr])

let labelCons (leftdtr, rightdtr) =
  let ll = label leftdtr in
  Node (T ("features",[(category_const Construct);   (* category *)
		       (subterm ll 1);               (* number   *)
		       (subterm ll 2);               (* person   *)
		       (subterm ll 3);               (* gender   *)
		       (subterm (label rightdtr) 4)  (* def      *)
		      ]), [leftdtr; rightdtr])


let rec s words =
  ((sub &. (np >. requireDef No))
      |. (sub &. adj)
        >. unifyGender >. assignCategory Sentence) words

and sub words =
  (pronoun |. (np >. requireDef Yes)) words

and np words =
  (aj |. cons |. n) words

and aj words =
  ((cons |. n) &. adjs >. unifyGenderDef >. assignCategory AdjP) words

and cons words =
  ((n >. requireDef No) &. n >. labelCons) words

and adjs words =
  (adj |. (adj &. adjs >. unifyGenderDef)) words

and pronoun = terminal Pronoun

and n = terminal Noun

and adj = terminal Adjective


let firstanswer w = try
		      fst (List.find (function (_,remainder) -> remainder = []) (s (lex w)))
  with Not_found -> failwith "found no analyses"
