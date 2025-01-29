(* CA - 1 *)

(* 1. You are required to write an Ocaml function (nofibo) which takes an integer list as
input, removes all integers from it which occur in the Fibonacci series, and returns
the remaining numbers as output. *)

let rec is_fibo n a b = 
  if n = a then true
  else if a > n then false
  else is_fibo n b (a+b);;

let rec nofibo lst = List.filter (fun x -> is_fibo x 0 1) lst;;
(* val nofibo : int list -> int list = <fun> *)


(* 2. You are aware that the * operator expects integers and *. operator expects floats in
Ocaml, and you are not allowed to mix their inputs (for example, 1 * 2.5 is not
allowed).
Now, using the appropriate techniques you have learnt, write a function
(fleximult) that will take two numbers (both ints or both floats or a mix of both) and
returns their product. The result is a float if one or both of the inputs is a float,
or an int if both inputs are ints. *)

type number = Int of int | Float of float;;

let fleximult x y = match (x, y) with
| (Int x, Int y) -> Int (x * y)
| (Int x, Float y) -> Float (float_of_int x *. y)
| (Float x, Int y) -> Float (x *. float_of_int y)
| (Float x, Float y) -> Float (x *. y);;
(* val fleximult : number -> number -> number = <fun> *)

(* Example :-  *)
fleximult (Int 3) (Int 4);;
fleximult (Int 3) (Float 2.8);;
fleximult (Float 1.4) (Float 3.7);;
fleximult (Float 5.3) (Int 2);;


(* 3. You are to write an OCaml function (comball) that takes a list of elements and returns
a list all possible of combinations of these elements. *)
let rec comball lst = match lst with
| [] -> [[]]
| x :: xs ->
    let rest = comball xs in
    rest @ List.map (fun sub -> x :: sub) rest;;
(* val comball : 'a list -> 'a list list = <fun> *)

comball [1;2;3;4];;


(* 2. You are given the following data type definition:
type nat = Zero | Succ of nat
Using this data type all natural numbers can be defined.
Now you are to write a function (addnat) that takes two 'nat's and returns their sum as a 'nat'. *)
type nat = Zero | Succ of nat

let rec addnat n1 n2 = match n1 with
| Zero -> n2
| Succ m -> Succ (addnat m n2);;
(* val addnat : nat -> nat -> nat = <fun> *)


(* CA - 2 *)

(* 3. Write an Ocaml module 'MatOps' that provides the following functionalities to the users of the module:
a) A data type ('a mymat) that represents a matrix as a list of list of 'a type elements.
b) A function 'make_mat' that takes two positive integers representing the number of rows and
cols in the resulting matrix and a list of elements. It should break the given list into
appropriate rows and cols to return the matrix as a 'list of list option' of the provided
elements. In case the matrix of the given size cannot be created, it should return None.
c) A function 'all' that takes a predicate and a matrix ('a mymat) and returns true if all
elements of the matrix satisfies the predicate, and false otherwise. You MUST use one of the fold
functions from the List library for writing this function.
d) A function 'exists' that takes a predicate and a matrix ('a mymat) and returns true if at
least one element of the matrix satisfies the predicate, and false otherwise. *)
type 'a mymat = 'a list list;;

let make_mat rows cols lst =
  let rec split lst n =
    if n <= 0 then ([], lst)
    else match lst with
      | [] -> ([], [])
      | hd :: tl ->
          let (row, rest) = split tl (n - 1) in
          (hd :: row, rest)
  in
  let rec chunk lst n =
    match lst with
    | [] -> []
    | _ ->
        let (row, rest) = split lst n in
        row :: chunk rest n
  in
  if List.length lst <> rows * cols then None
  else Some (chunk lst cols);;

let all predicate mat =
  List.for_all (fun row -> List.for_all predicate row) mat;;

let exists predicate mat =
  List.exists (fun row -> List.exists predicate row) mat;;

(* Complete MatOps module :-  *)
(* module MatOps = struct
  type 'a mymat = 'a list list

  let make_mat rows cols lst =
  let rec split lst n =
    if n <= 0 then ([], lst)
    else match lst with
      | [] -> ([], [])
      | hd :: tl ->
          let (row, rest) = split tl (n - 1) in
          (hd :: row, rest)
  in
  let rec chunk lst n =
    match lst with
    | [] -> []
    | _ ->
        let (row, rest) = split lst n in
        row :: chunk rest n
  in
  if List.length lst <> rows * cols then None
  else Some (chunk lst cols)

  let all predicate mat =
    List.for_all (fun row -> List.for_all predicate row) mat

  let exists predicate mat =
    List.exists (fun row -> List.exists predicate row) mat
end;; *)

module type MATOPS = sig
  type 'a mymat

  val make_mat : int -> int -> 'a list -> 'a mymat option
  val all : ('a -> bool) -> 'a mymat -> bool
  val exists : ('a -> bool) -> 'a mymat -> bool
end;;


(* CA - 3 *)

(* 4. You are given the type definition: type student = {roll:int; name: string}
(a) Write Ocaml code to create a StudentMap module which can be used to create a map (which is
known as the dict type in Python, i.e., a key value store) which can hold a student type as key
and an integer (indicating student age) as the value associated with the key.
*Note: This also requires you to write any additional module(s) if needed.* *)
type student = { roll : int; name : string };;

module Student = struct
  type t = student
  let compare s1 s2 = compare s1.roll s2.roll
end;;

module StudentMap = Map.Make(Student);;

(* (b) Write Ocaml code to add following 2 student information to a map (as mentioned in 4(a) above)
named smap: student#1: Suresh, Roll=100, Age=20; student#2: Nabin, Roll=99, Age=23 *)
let st1 = {roll = 100; name = "Suresh"};;
let st2 = {roll = 99; name = "Nabin"};;

let smap = StudentMap.(empty |> add st1 20 |> add st2 23);;
(* val smap : int StudentMap.t = <abstr> *)

StudentMap.bindings smap;;
(* - : (student * int) list =
[({roll = 99; name = "Nabin"}, 23); ({roll = 100; name = "Suresh"}, 20)] *)

(* 5. (a) Write an Ocaml function make_array that takes a list of integers and returns an array
containing those integers. Do NOT use any library function other than Array.init and List.length.
(b) Write an Ocaml function mod_array that satisfies the following requirements:
*input*: an array containing N integers i1, i2, i3, ... iN; *output*: unit
The function modifies the provided array to contain the integers i1, (i1+i2), (i1+i2+i3),
... (i1+i2+...+iN).
Note: You may use the Array.init function to initialize an array (val Array.init: int -> (int -> 'a) -> 'a array). 
For example, Array.init 10 (fun i -> 0) will create the array of size 10 containing all 0s. 
The Array.length function takes an array and returns the length of the array. *)

let make_array lst = Array.init (List.length lst) (fun x -> List.nth lst x);;
(* val make_array : 'a list -> 'a array = <fun> *)

let mod_array arr =
  let len = Array.length arr in
  for i = 1 to len - 1 do
    arr.(i) <- arr.(i) + arr.(i - 1)
  done;;
(* OR *)
let mod_array1 arr =
  let n = Array.length arr in
  let rec accumulate i sum =
    if i < n then begin
      arr.(i) <- arr.(i) + sum;
      accumulate (i + 1) arr.(i)
    end
  in
  accumulate 0 0;;
