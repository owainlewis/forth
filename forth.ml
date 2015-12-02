(* --------------------------------------------------------------------- *)
(*                                                                       *)
(* Forth.ml #load "str.cma"                                              *)
(*                                                                       *)
(* A micro compiler for a small subset of the FORTH programming language *)
(*                                                                       *)
(* --------------------------------------------------------------------- *)

open MutableStack

(* Rename to Env *)
module Assoc = struct  
    let procs = ref [ ]
    let insert k v =
      let existing = !procs in
        procs := (k, v) :: existing

    let find p = List.assoc p !procs
end

(***********************************************)
(* STACK OPS *)
(***********************************************)

let init = MutableStack.empty

let swap stack =
  let a = MutableStack.pop stack
  and b = MutableStack.pop stack in
  let _ = List.iter (fun x -> MutableStack.push stack x) [ a; b ] in
  !stack

(* Duplicate the next item on the stack *)
let dup stack =
  let element = MutableStack.peek stack in
  match element with
  | Some(e) -> MutableStack.push stack e
  | None    -> ()

(***********************************************)

(* FORTH operators *)
type token =
    ADD
  | SUB
  | MULT
  | DIV
  | DUMP
  | DUP
  | SWAP
  | DOT
  | INT  of int
  | ATOM of string

(* Show a token *)
let show_token = function
    ADD    -> "+"
  | SUB    -> "-"
  | MULT   -> "*"
  | DIV    -> "/"
  | DUMP   -> "DUMP"
  | DUP    -> "DUP"
  | SWAP   -> "SWAP"		
  | DOT    -> "."
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
  | "-"    -> SUB
  | "*"    -> MULT
  | "/"    -> DIV
  | "DUMP" -> DUMP
  | "DUP"  -> DUP
  | "SWAP" -> SWAP		
  | "."    -> DOT
  | x   -> match (try_int x) with
           | Some(i) -> INT i
           | None    -> ATOM x

(* Lex the input into actionable tokens *)
let lex input =
  let tokenize = Str.split (Str.regexp " ") in
  let rec aux lexed = function
      []      -> lexed
    | (x::xs) -> aux (lex_token x :: lexed) xs
  in
    let lex_result =
      aux [] (tokenize input) in
    List.rev lex_result

(* Operations and FORTH built in functions *)

let dump stack =
  let token_strings =
     (List.map show_token !stack)
     |> List.rev in
  let parts =
     (String.concat " " token_strings)
  in print_endline ( "[ " ^ parts ^ " ]" )

let pop2 stack =
  let a = MutableStack.pop stack
  and b = MutableStack.pop stack in
  (a,b)
		   
let apply_prim_op op stack =
  match (pop2 stack) with
  | (INT x, INT y) -> MutableStack.push stack (INT (op x y))
  | _              -> raise (Failure "Incompatible types for operation")

let add stack  = apply_prim_op (fun x y -> x + y) stack
and sub stack  = apply_prim_op (fun x y -> x - y) stack
and mult stack = apply_prim_op (fun x y -> x * y) stack
and div stack  = apply_prim_op (fun x y -> x / y) stack

let print_forth line = print_endline (line ^ "\nok.")

(* Pop an item off the top of the stack and print it *)
let dot stack =
  let element = MutableStack.pop stack in
  print_forth (show_token element)

let run stack program =
  let tokens = lex program in
  let _ =
    List.iter
      (fun c -> match c with
                | ADD   -> add(stack)
                | SUB   -> sub(stack)
                | MULT  -> mult(stack)
                | DIV   -> div(stack)
                | DUMP  -> dump(stack)
                | DUP   -> dup(stack)
                | DOT   -> dot(stack)
                | _     -> MutableStack.push stack c)
      tokens
  in stack

(* RUN A PROCEDURE *)
let run_proc stack name =
    let procedure = Assoc.find name in
    let _ = run stack procedure in
    stack

let go program = run (MutableStack.empty()) program

