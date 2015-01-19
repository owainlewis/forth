(* --------------------------------------------------------------------- *)
(*                                                                       *)
(* Forth.ml                                                              *)
(* A micro compiler for a small subset of the FORTH programming language *)
(*                                                                       *)
(* --------------------------------------------------------------------- *)

(* Custom stack instance of OCaml native Stack module
   to add debugging methods etc *)
module MStack =
  struct
    type 'a mstack = ('a list) ref
    let empty () = ref []
    let push stack value : unit =
      let current_stack = !stack in
      stack := (value :: current_stack)
    let pop stack =
      match (!stack) with
      | [] -> raise (Failure "Empty stack")
      | x::xs -> (stack := xs); x
  end

(* FORTH operators *)
type token =
    ADD
  | SUB
  | MULT
  | DIV
  | DUP
  | DOT
  | SWAP
  | INT  of int
  | ATOM of string

let tokenize = Str.split (Str.regexp " ")

let lex_token = function
  | "+" -> ADD
  | "*" -> MULT
  | "/" -> DIV
  | x   -> match (try_int x) with
           | Some(i) -> INT i
           | None -> ATOM x

and try_int token =
  try
    Some(int_of_string token)
  with _ -> None

and try_float token =
  try
    Some(float_of_string token)
  with _ -> None

(* Lex the input into actionable tokens *)
let lex input =
  let rec aux lexed = function
      []      -> lexed
    | (x::xs) -> aux (lex_token x :: lexed) xs
  in let lex_result = aux [] (tokenize input) in
  List.rev lex_result

(* Operations and FORTH built in functions *)

(* Apply the + function to our stack *)
let add stack =
  let a = MStack.pop stack
  and b = MStack.pop stack in
  match (a,b) with
  | (INT x, INT y) -> MStack.push stack (INT (x + y))
  | _              -> raise (Failure "Incompatible types for addition operator")

let run stack input =
  let tokens = lex input in
  let _ = List.iter (fun c ->
              if c = ADD then add(stack)
              else MStack.push stack c) tokens
  in stack

let repl input stack =
  let result = run stack input in
  result
