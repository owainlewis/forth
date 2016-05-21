(* --------------------------------------------------------------------- *)
(*                                                                       *)
(* Forth.ml #load "str.cma"                                              *)
(*                                                                       *)
(* A micro compiler for a small subset of the FORTH programming language *)
(*                                                                       *)
(* --------------------------------------------------------------------- *)

open Printf

(* module Env = struct *)
(*   let env = ref [ ] *)
(*   let insert k v = *)
(*     let existing_env = !env in *)
(*     env := (k, v) :: existing_env *)
(*   let find p = *)
(*     try *)
(*       let v = List.assoc p !env in Some(v) *)
(*     with Not_found -> None *)
(* end *)

exception UnderflowException of string

module Stack = struct
  type 'a stack = 'a list ref
  (* Create an empty stack *)
  let empty () = ref []
  (* Peek the top of the stack *)
  let peek stack = match !stack with
  | [] -> None
  | x :: _ -> Some(x)
  (* Push an item onto the stack *)
  let push stack value =
    let new_stack = (value :: !stack) in
    stack := new_stack
  (* Pop an item off the stack. Unsafe *)
  let pop stack = match (!stack) with
  | []    -> raise (UnderflowException "Stack underflow")
  | x :: xs -> let _ = stack := xs in x
  (* Initialize the stack with some values *)
  let init_with xs = ref xs
  (* Dump the results of the stack for inspection *)
  let inspect stack = !stack
end

(*****************************************************************)
(* Lexing + AST                                                  *)
(*****************************************************************)

(* FORTH operators *)
type token =
  | Int  of int
  | String of string
  | Word of string

(* Show a token *)
let show_token = function
  | Int i  -> string_of_int i
  | String x -> x
  | Word s -> s

let try_int token =
  try
    Some(int_of_string token)
  with _ -> None

let try_float token =
  try
    Some(float_of_string token)
  with _ -> None

let lex_token = function
  | x   -> match (try_int x) with
           | Some(i) -> Int i
           | None    -> Word x

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

let dump stack =
  let token_strings =
     (List.map show_token !stack)
     |> List.rev in
  let parts =
     (String.concat " " token_strings)
  in print_endline ( "[ " ^ parts ^ " ]" )

(*****************************************************************)
(* Base                                                          *)
(*****************************************************************)

let apply_prim_op op stack =
  let a = Stack.pop stack in
  let b = Stack.pop stack
  in
  match ((a,b)) with
  | (Int x, Int y) -> Stack.push stack (Int (op x y))
  | _              -> raise (Failure "Incompatible types for operation")

let add stack  = apply_prim_op (fun x y -> x + y) stack
and sub stack  = apply_prim_op (fun x y -> x - y) stack
and mult stack = apply_prim_op (fun x y -> x * y) stack
and div stack  = apply_prim_op (fun x y -> x / y) stack

let swap (stack : 'a list ref) =
  let a = Stack.pop stack
  and b = Stack.pop stack in
  List.iter (Stack.push stack) [ a; b ];
  !stack

let dup stack = match (Stack.peek stack) with
               | Some(e) -> Stack.push stack e
               | None    -> ()

let dot stack =
  let print_forth line = print_endline (line ^ "\nok.") in
  let element = Stack.pop stack in
  print_forth (show_token element)

(*****************************************************************)
(* Eval                                                          *)
(*****************************************************************)

let eval stack program =
  let rec aux stack program =
    match program with
      | [] -> stack
      | (x :: xs) ->
        match x with
          (* Just push integers onto the stack *)
          | Int n -> Stack.push stack (Int n); aux stack xs
          | String s -> Stack.push stack (String s); aux stack xs
          | Word w -> match w with
                      | "+" -> add stack; aux stack xs
                      | "*" -> mult stack; aux stack xs
                      | "-" -> sub stack; aux stack xs
                      | "/" -> div stack; aux stack xs
                      | "dup" -> dup stack; aux stack xs
                      | "swap" -> swap stack; aux stack xs
                      | "." -> dot stack; aux stack xs
                      | _ -> aux stack xs
          | _ -> aux stack xs
  in aux stack program

let parse program =
  let tokens = lex program in
  let stack = Stack.empty() in
  let _ =
    List.iter
      (fun c -> match c with
               | _     -> Stack.push stack c)
      tokens
  in List.rev (Stack.inspect stack)

let go stack program = eval stack (parse program)

(* Main *)

let spacer = print_endline "---------------------------------------"

let repl stack =
  let out = Printf.printf "%s\n" in
  try
    while true do
      let line = input_line stdin in
      spacer;
      go stack line;
      spacer;
      out line
    done;
  with UnderflowException e -> ()

let () =
  spacer;
  print_endline "<< FORTH: A simple forth interpreter >>";
  spacer;
  repl (Stack.empty())
