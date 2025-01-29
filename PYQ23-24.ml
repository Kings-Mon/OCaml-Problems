(* Write an OCaml module to represent a Complex number that implements add, subtract and multiply operations 
on complex numbers (those that have a real and an imaginary part, both of which are integers for this case) *)

type nums = { real : int; imag : int };;

module ComplexImp = struct
  type t = nums
  let create real imag = { real; imag }

  let add c1 c2 = { real = c1.real + c2.real; imag = c1.imag + c2.imag }

  let subtract c1 c2 = { real = c1.real - c2.real; imag = c1.imag - c2.imag }

  let multiply c1 c2 =
    {
      real = (c1.real * c2.real) - (c1.imag * c2.imag);
      imag = (c1.real * c2.imag) + (c1.imag * c2.real);
    }
end;;   (* For multiply - (a+bi)∗(c+di)=(a∗c−b∗d)+(a∗d+b∗c)i *)

(* Example - ComplexImp.add (ComplexImp.create 3 4) (ComplexImp.create 1 5);; *)



(* Use the following type for representing a sequence (for this question):
Define a value pow2: int seg whose elements are powers of two: 1, 2, 4,... *)
type 'a seq = Cons of 'a * (unit -> 'a seq);;

let pow2 = 
  let rec generate n = Cons (n, fun () -> generate (n * 2)) 
in generate 1;;

(* Example Usage *)
let rec take n (Cons(x, rest)) = match n with
| 0 -> []
| _ -> x :: take (n - 1) (rest());;

take 10 pow2;;


(* Use the following type for representing a sequence (for this question):
Define a value abc: char seg whose elements are: a,b,c,a,b,c,a,.... *)
(* type 'a seq = Cons of 'a * (unit -> 'a seq);; *)

let abc = 
  let rec generate char_lst = match char_lst with
  | [] -> generate ['a';'b';'c']
  | x :: rest -> Cons(x, fun () -> generate rest) 
in generate ['a';'b';'c'];;

(* Example Usage *)
let rec take n (Cons(x, rest)) = match n with
| 0 -> []
| _ -> x :: take (n - 1) (rest());;
take 7 (abc);;



(* i. Define an OCaml variant type for infinite lists, without the possibility of finite lists.
Briefly illustrate programming techniques for your variant type by declaring:
ii. A recursive function (analogous to map for ordinary lists) that applies a given function to every element of an infinite list.
iii. A function for generating infinite lists of the form x, f(x), f(f(x)), ... for any given f and x. *)
type 'a infi_lst = Cons of 'a * (unit -> 'a infi_lst);;

let rec map f (Cons (x, next)) = 
  Cons (f x, fun () -> map f (next()));;

let rec iterate f x = Cons (x, fun () -> iterate f (f x));;


(* The OCaml variant type bool, defined below, is to be used to represent boolean expressions.
type bool = Var of string
| Not of bool
| And of bool * bool
| Or of bool * bool
The constructor Var is used to represent named boolean variables, and Not. And and Or are used to represent the corresponding boolean operators.
i. Define a function that will return a list of the distinct names used in a boolean expression.
ii. A context is represented by a list of strings corresponding to the boolean variables that are set to true. All other variables are deemed to be set to false.
Define a function that will evaluate a given boolean expression in a given context *)
type bool = Var of string
| Not of bool
| And of bool * bool
| Or of bool * bool;;

let rec vars expr = match expr with
| Var name -> [name]
| Not e -> vars e
| And (e1, e2) | Or (e1, e2) -> vars e1 @ vars e2;;

(* Remove duplicates *)
let distinct_vars expr =
  let rec remove_duplicates lst seen =
    match lst with
    | [] -> []
    | x :: xs ->
        if List.mem x seen then remove_duplicates xs seen
        else x :: remove_duplicates xs (x :: seen)
  in
  remove_duplicates (vars expr) [];;


let rec eval expr context =
  match expr with
  | Var name -> List.mem name context  (* True if the variable is in the context *)
  | Not e -> not (eval e context)
  | And (e1, e2) -> eval e1 context && eval e2 context
  | Or (e1, e2) -> eval e1 context || eval e2 context;;


(* i. Write a function clip which, given an integer, clips it to the range 1... 10 
so that integers bigger than 10 round down to 10 and integers smaller than 1 round up to 1.
ii. Write another function cliplist which uses clip together with map to apply this clipping to a whole list of integers.
iii. Write a function mapl which maps a function of type 'a->'b over a list of type 'a list list to produce a list of type 'b list list *)

let clip x = 
  if x < 1 then 1
  else if x > 10 then 10
  else x;;

let cliplist lst = List.map clip lst;;

let mapl f lst_lst = List.map (List.map f) lst_lst;;


(* For this question 'a seq is the type defined as follows:
type 'a seq = Cons of 'a * (unit -> 'a seq)
and nats is the sequence of natural numbers 0,1,2,3,...
i. Define a function filter ('a->bool)->'a seq->'a seq such that filter p s is the sub-sequence of s whose elements satisfy the predicate p. 
For example, filter (fun n -> n mod 2 = 0) nats would be the sequence 0,2,4,6,8,10,.... If there is no element of s that satisfies p, then filter p s does not terminate. 
ii. Define a function interleave : 'a seq -> 'a seq -> 'a seq, such that interleave <a1;a2;a3...> <b1;b2;b3...> is the sequence <a1;b1;a2;b2;a3;b3;...> *)
type 'a seq = Cons of 'a * (unit -> 'a seq);;

let rec filter p (Cons (x, next)) = 
  if p x then Cons(x, fun() -> filter p (next()))
  else filter p (next());;

let rec interleave (Cons (x, next1)) (Cons (y, (next2))) = 
  Cons (x, fun () -> Cons (y, fun () -> interleave (next1()) (next2())));;
