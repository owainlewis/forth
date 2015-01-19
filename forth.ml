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
  | DUMP
  | INT  of int
  | ATOM of string

(* Show a token *)
let show_token = function
    ADD    -> "+"
  | SUB    -> "-"
  | MULT   -> "*"
  | DIV    -> "/"
  | DUMP   -> "DUMP"
  | INT i  -> string_of_int i
  | ATOM s -> s

let try_int token =
  try
    Some(int_of_string token)
  with _ -> None

let try_float token =
  try
    Some(float_of_string token)
  with _ -> None

let lex_token = function
  | "+"    -> ADD
  | "*"    -> MULT
  | "/"    -> DIV
  | "DUMP" -> DUMP
  | x   -> match (try_int x) with
           | Some(i) -> INT i
           | None    -> ATOM x

(* Lex the input into actionable tokens *)
let lex input =
  let tokenize = Str.split (Str.regexp " ") in
  let rec aux lexed = function
      []      -> lexed
    | (x::xs) -> aux (lex_token x :: lexed) xs
  in let lex_result = aux [] (tokenize input) in
  List.rev lex_result

(* Operations and FORTH built in functions *)

let dump stack =
  let token_strings = (List.map show_token !stack) in
  let parts         = (String.concat " " token_strings) in
  print_endline ( "[ " ^ parts ^ " ]" )

(* Apply the + function to our stack *)
let add stack =
  let a = MStack.pop stack
  and b = MStack.pop stack in
  match (a,b) with
  | (INT x, INT y) -> MStack.push stack (INT (x + y))
  | _              -> raise (Failure "Incompatible types for addition operator")

let run stack input =
  let tokens = lex input in
  let _ =
    List.iter
      (fun c -> match c with
                | ADD  -> add(stack)
                | DUMP -> dump(stack)
                | _    -> MStack.push stack c) tokens
  in stack

let repl input =
  let stack = MStack.empty() in
  let result = run stack input in
  ()
