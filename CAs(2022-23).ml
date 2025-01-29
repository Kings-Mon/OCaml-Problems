(*Q1. 
(a) Given the following type ('nat') for natural numbers, write an ocaml function that takes two natural numbers (n1 and n2) 
and returns 0, -1, 1 if n1=n2, n1<n2, n1>n2 respectively.
#+begin_src ocaml
type nat = Zero | Succ of nat
#+end_src
(b) Write the type of the function written by you.
(c) What kind of type is 'nat'?*)
type nat = Zero | Succ of nat

let rec compare_nats n1 n2 = match (n1, n2) with
| (Zero, Zero) -> 0
| (Zero, _) -> -1
| (_, Zero) -> 1
| (Succ m1, Succ m2) -> compare_nats m1 m2 ;;
(* OR *)
let nat_compare (n1:nat) (n2:nat) : int = 
  if n1 = n2 
    then 0 
  else if n1 > n2 
    then 1 
  else -1 ;;

(*val compare_nats : nat -> nat -> int = <fun>*)     (*<-- Type of the function*)

(* The type 'nat' represents natural numbers using a recursive algebraic data type. 
It is a user-defined data type.*)


(*Q2. 
(a) Write an Ocaml function to get the nth item from a list without using any List library function. 
Your function must be able to use any kind of list type.
(b) Write the type of the function you wrote above (in 2(a)).*)

let rec nth_item lst n = match lst with
| [] -> 0
| h :: t ->
  if n <= 0 then 0 
  else if n = 1 then h
  else nth_item t (n - 1) ;;
(* OR *)
let rec nth_item lst n = match lst, n with 
| [], _ -> failwith "Index out of bounds"
| h :: t, 0 -> h 
| h :: t, _ -> nth_item t (n-1) ;;

(*val nth_item : int list -> int -> int = <fun>*)     (*<-- Type of the function*)

(* Using List library function :- *)
let nth_item lst n =
  if List.length lst >= n 
    then List.nth lst (n-1)
  else 0 ;;
(*val nth_item : int list -> int -> int = <fun>*)


(*Q3. 
Modify the code in 2(a) above so that your function returns an option 
of the list element type in order to properly handle the empty list case.*)
let rec nth_item1 n lst = match lst with
| [] -> None
| hd :: tl -> if n = 1 then Some hd else nth_item1 (n-1) tl ;;
(* OR *)
let rec nth_item1 n lst = match n, lst with
  | 0, x :: _ -> Some x
  | _, [] -> None
  | n, _ :: t -> nth_item1 (n-1) t ;;
(*val nth_item1 : int -> 'a list -> 'a option = <fun>*)

(* Using List library function :- *)
let nth_item1 n lst =
  if n < 0 || lst = [] then None
  else
    try Some (List.nth lst n)
    with Invalid_argument _ -> None ;;
(*val nth_item1 : int -> 'a list -> 'a option = <fun>*)

(*Q4. Write the types of - List.map, List.fold_right and List.fold_left, List.filter*)
List.map;; (*'a -> ('a -> 'b) -> 'b list*)
List.fold_right;; (*'a -> ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b*)
List.fold_left;; (*'a -> ('b -> 'a -> 'b) -> 'b -> 'a list -> 'b*)
List.filter;; (*'a -> ('a -> bool) -> 'a list -> 'a list*)


(*5.
You are to write a module that provides functionality to add two matrices of integers. 
But the problem is, you need to be able to reuse the same code to be able to add two matrices of floats as well.
(a) Write ocaml code to solve this problem with the help of a functor.
(b) Write ocaml code to demonstrate how you would use your solution in 1(a) above to add two int matrices, and two float matrices. *)

module type NUMERIC = sig
  type t
  val add : t -> t -> t
end;;

module IntOps : NUMERIC with type t = int = struct
  type t = int
  let add x y = x + y
end;;

module FloatOps : NUMERIC with type t = float = struct
  type t = float
  let add x y = x +. y
end;;

module MatrixOps (N : NUMERIC) = struct
  let add_matrices m1 m2 =
    List.map2 (fun row1 row2 ->
      List.map2 N.add row1 row2
    ) m1 m2
end;;

module IntMatrix = MatrixOps(IntOps);;
module FloatMatrix = MatrixOps(FloatOps);;

(*Example - *)
let int_matrix1 = [[1; 2; 3]; [4; 5; 6]];;
let int_matrix2 = [[7; 8; 9]; [10; 11; 12]];;
let result_int = IntMatrix.add_matrices int_matrix1 int_matrix2;;
(* result_int = [[8; 10; 12]; [14; 16; 18]] *)

let float_matrix1 = [[1.0; 2.0; 3.0]; [4.0; 5.0; 6.0]];;
let float_matrix2 = [[7.0; 8.0; 9.0]; [10.0; 11.0; 12.0]];;
let result_float = FloatMatrix.add_matrices float_matrix1 float_matrix2;;
(* result_float = [[8.0; 10.0; 12.0]; [14.0; 16.0; 18.0]] *)


(*6.
Given a sequence s, write a function seq_fold to generate a new sequence out of it,
such that the nth item of this new sequence is the fold of the first n items of s. *)

let seq_fold s =
  let rec helper acc s =
    match s with
    | [] -> []
    | x :: xs ->
      let new_acc = acc + x in
      new_acc :: helper new_acc xs
  in
  helper 0 s;;
(*val seq_fold : int list -> int list = <fun>*)

(* Example - *)
seq_fold [1;2;3;4;5];;
(*- : int list = [1; 3; 6; 10; 15]*)


(* 7.
The thunk based version of fibonacci sequence discussed in class was inefficient in terms of speed due to lots of redundant computations. 
Find a way to remove this redundancy without using the lazy evaluation facility of Ocaml. 
Hence write an efficient re-implementation of the fibonacci sequence. *)

let fib n =
  let rec helper a b count =
    if count = n then a
    else helper b (a + b) (count + 1)
  in
  helper 0 1 0;;
(* val fib : int -> int = <fun> *)


(*8.
Instead of writing specific code for every sequence required, 
we can abstract out the sequence generating function f (the code that generates the next number in the sequence) 
and pass it, along with the initial value of the sequence (call it x) to a function make_seq that will create the sequence for us. 
Write Ocaml code for this make_seq function.*)

let rec make_seq f x =
  let rec generate x = 
    x :: generate (f x)
  in
  generate x;;
(* val make_seq : ('a -> 'a) -> 'a -> 'a list = <fun> *)


