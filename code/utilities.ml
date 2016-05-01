(* utilities for computational linguistics class 2013
   prepared by John Hale <jthale@cornell.edu>
*)


(* back and forth from characters to string to char lists *)
let explode s =
  let rec explode s i = if i < String.length s
  then s.[i]::(explode s (i+1))
  else []
  in (explode s 0)

let implode l =
  begin
    let v = (String.create (List.length l)) in
      for i = 0 to ((List.length l)-1) do  (* impure! *)
	String.set v i (List.nth l i)
      done; v
  end


(* apply the one-place function f to an argument x *)
(* as a side effect, print out the time it took *)
let time f x =
(*   let time0 = (Unix.times()).Unix.tms_utime in *)
  let time0 = Sys.time () in
  let result = f x in
  let time1 = Sys.time ()in
(*  let time1 = (Unix.times()).Unix.tms_utime in *)
  Printf.printf "%2.2f seconds\n" ((time1 -. time0));
  result


let write_file fname str =
  let oc = open_out fname in
    begin
      output_string oc str;
      close_out oc
    end


(* the function range helps with getting lists of state names *)
(* i.e. List.fold_left (fun set elt -> IntSet.add elt set) IntSet.empty (range 0 10) *)
let range start finish =
  let rec rangeAux current =
    match (finish-current) with
        0 -> [current]
      | _ -> current::(rangeAux (current+1))
  in
    if finish < start
    then []
    else  rangeAux start


(* divides a string s into a list of substrings at the points where it finds any of the *)
(* characters on the `splitters' list *)
(* example: lex "hello out there"   --->    ["hello"; "out"; "there"] *)
let split_on splitters s =
  let rec tok remainingchars most_recent_word_accumulator = match remainingchars,most_recent_word_accumulator with
    [], w -> [implode (List.rev w)]
    | (c::cs), w ->
        if (List.mem c splitters)
        then (implode (List.rev w))::(tok cs [])
        else tok cs (c::w)
  in
  tok (explode s) []


let split x = List.map (String.make 1) (explode x)
let split_ws = split_on [' ';'\t';'\n']
let split_plus = split_on ['+']
(* lex, a low-performance lexer *)
let lex = split_ws


(* a fold function that applies f line-by-line to a file *)
let fold_file filename f initial =

  (* return a pair with the property that if the first component is true *)
  (* then the second component is a good line of input. Otherwise, we ran out of lines *)
  let nextline channel =
    try
      true,(input_line channel)
    with End_of_file -> false,""  in

  (* apply opr : operand -> data -> updated_data  to each line of the input channel *)
  let rec repeat chan opr sofar = match (nextline chan) with
      (true,line) -> repeat chan opr (opr line sofar)   (* "sofar := (opr line sofar)" *)
    | (false,_) -> sofar   in

  let infile = open_in filename in
  let result = repeat infile f initial in
  let _ = close_in infile in
  result;;

(* compose two one-place functions, f and g *)
let ( $ ) f g x = f (g x)


(* maintains set invariant in polymorphic lists *)
let append_unique l1 l2 =
  let addable = List.fold_left (fun old this -> if List.mem this l2 then old else this::old) [] l1 in
  List.rev_append addable l2
