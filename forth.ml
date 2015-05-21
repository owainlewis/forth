(* --------------------------------------------------------------------- *)
(*                                                                       *)
(* Forth.ml                                                              *)
(*                                                                       *)
(* A micro compiler for a small subset of the FORTH programming language *)
(*                                                                       *)
(* --------------------------------------------------------------------- *)

(* Custom range function *)
let rng n =
  let rec aux xs i =
    if i <> n then
      aux (xs @ [i]) (i+1)
    else xs
  in
  if n < 0 then [] (* Negative range *)
           else aux [] 0

let range_iter f n = List.iter f (rng n)
let range_map  f n = List.map f (rng n)

(* Custom stack instance of OCaml native Stack module
   to add debugging methods etc *)
module MStack = struct

  type 'a mstack = ('a list) ref

  let empty () = ref []

  let peek stack =
    match !stack with
    | []    -> None
    | x::xs -> Some(x)

  let push stack value : unit =
    stack := (value :: !stack)

  let pop stack =
    match (!stack) with
    | []    -> raise (Failure "Empty stack")
    | x::xs -> (stack := xs); x
end

(* FORTH operators *)
type token =
    ADD
  | SUB
  | MULT
  | DIV
  | DUMP
  | DUP
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

(* Apply a simple math operation to integer types. Must make this polymorphic for floats
   etc *)
let apply_prim_op op stack =
  let a = MStack.pop stack
  and b = MStack.pop stack in
  match (a,b) with
    | (INT x, INT y) -> MStack.push stack (INT (op x y))
    | _              -> raise (Failure "Incompatible types for operator")

let add stack  = apply_prim_op (fun x y -> x + y) stack
and sub stack  = apply_prim_op (fun x y -> x - y) stack
and mult stack = apply_prim_op (fun x y -> x * y) stack
and div stack  = apply_prim_op (fun x y -> x / y) stack

let swap stack =
  let a = MStack.pop stack
  and b = MStack.pop stack in
  List.iter (MStack.push stack) [b;a]

(* Duplicate the next item on the stack *)
let dup stack =
  let element = MStack.peek stack 
  in
  match element with
  | Some(e) -> MStack.push stack e
  | None    -> ()

let print_forth line = print_endline (line ^ "\nok.")

(* Pop an item off the top of the stack and print it *)
let dot stack =
  let element = MStack.pop stack in
  print_forth (show_token element)

let run stack input =
  let tokens = lex input in
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
                | _     -> MStack.push stack c)
      tokens
  in stack

let repl input =
  let stack = MStack.empty() in
  let result = run stack input in
  (* Eventually we just want to return void here *)
  !result
