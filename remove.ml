(*1. A function, remove (s: string) (los: string list) : string list 
which removes all occurances of s from los.*)
let rec remove s los = match los with
| [] -> []
| h :: t -> 
	if h = s then 
		remove s t 
	else 
		h :: remove s t;;
(*val remove : 'a -> 'a list -> 'a list = <fun>*)
(*Alternative - *)
let remove s lst = List.filter (fun str -> str <> s) lst;;
(*val remove : 'a -> 'a list -> 'a list = <fun>*)

(*Example :- *)
remove "apple" ["apple"; "banana"; "cherry"; "apple"; "date"];;
(* - : string list = ["banana"; "cherry"; "date"] *)
remove "kingshuk" ["ami"; "tumi"; "kingshuk"; "se"];;


(*2.A function, 
remove2 (ts: string list) (ss: string list) : string list 
which removes all occurances of strings in ts from ss.*)
let rec remove2 ts sl = match sl with
| [] -> []
| h :: t -> 
	if List.mem h ts then remove2 ts t 
	else h :: remove2 ts t;;
(*val remove2 : 'a list -> 'a list -> 'a list = <fun>*)

(*Example :- *)
remove2 ["apple"; "date"] ["apple"; "banana"; "cherry"; "apple"; "date"; "grape"];;
(* - : string list = ["banana"; "cherry"; "grape"] *)
remove2 ["kingshuk"; "ami"] ["ami"; "tumi"; "kingshuk"; "o"; "se"; "kingshuk"];;


(*3. A function, make_histogram (sl: string list) : (string * int) list 
which finds out how many times each string in sl occurs in sl and returns a list of (string * int) 
where the int indicating how many times a particular string has appeared.*)
let rec make_histogram sl = 
	let rec helper str int = function
	| [] -> (str, int)
	| h :: t -> 
		if h = str then 
			helper str (int + 1) t 
		else 
			helper str int t
	in 
	match sl with 
	| [] -> []
	| h :: t ->
		let count = helper h 1 t in 
			count :: make_histogram (remove h t);;
(*val make_histogram : 'a list -> ('a * int) list = <fun>*)
(*Example :- *)
make_histogram ["kingshuk"; "ami"; "ami"; "tumi"; "o"; "kingshuk"; "se"; "kingshuk"];;
(*- : (string * int) list = [("kingshuk", 3); ("ami", 2); ("tumi", 1); ("o", 1); ("se", 1)]*)

(* It shows how many times the first list elements have occured in the second list. *)
let rec make_histogram1 sl_1 sl_2 = 
  let rec helper str int = function
  | [] -> (str, int)
  | h :: t -> if h = str then helper str (int + 1) t else helper str int t
  in 
  match sl_1 with 
  | [] -> []
  | h :: t ->
    let count = helper h 0 sl_2 in count :: make_histogram1 t sl_2;;
(*val make_histogram1 : 'a list -> 'a list -> ('a * int) list = <fun>*)

make_histogram1 ["ami"; "kingshuk"] ["kingshuk"; "ami"; "ami"; "tumi"; "o"; "kingshuk"; "se"; "kingshuk"];;
(*- : (string * int) list = [("ami", 2); ("kingshuk", 3)]*)

