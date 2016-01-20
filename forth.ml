(* --------------------------------------------------------------------- *)
(*                                                                       *)
(* Forth.ml #load "str.cma"                                              *)
(*                                                                       *)
(* A micro compiler for a small subset of the FORTH programming language *)
(*                                                                       *)
(* --------------------------------------------------------------------- *)

module Env = struct  
  let env = ref [ ]
  let insert k v =
    let existing_env = !env in
    env := (k, v) :: existing_env				   
  let find p = List.assoc p !env
end

exception UnderflowException of string
			
module Stack = struct
  type 'a stack = 'a list ref		     
  (* Create an empty stack *)			    
  let empty () = ref []		     
  (* Peek the top of the stack *)
  let peek stack = match !stack with
  | [] -> None
  | x::_ -> Some(x)		  
  (* Push an item onto the stack *)		  
  let push stack value =
    let new_stack = (value :: !stack) in
    stack := new_stack	       
  (* Pop an item off the stack. Unsafe *)	       
  let pop stack = match (!stack) with
  | []    -> raise (UnderflowException "Stack underflow")
  | x::xs -> let _ = stack := xs in x
  (* Initialize the stack with some values *)					
  let init_with xs = ref xs		   
  (* Dump the results of the stack for inspection *)					 
  let inspect stack = !stack
end

(*****************************************************************)
(* STACK OPS *)
(*****************************************************************)

(* Swap the top two elements on the stack *)		 
let swap (stack : 'a list ref) =
  let a = Stack.pop stack
  and b = Stack.pop stack 
  and f = Stack.push stack in 
  let _ = List.iter f [ a; b ] in
  !stack
(* Duplicate the next item on the stack *)
let dup stack =
  let element = Stack.peek stack in
  match element with
  | Some(e) -> Stack.push stack e
  | None    -> ()
(* Drop an item from the stack *)
let drop stack =
  let drop_head = function
    | [] -> []
    | x::xs -> xs	      
  in let updated = List.rev (drop_head (List.rev !stack)) in ref updated
								 
let over stack = stack
let rot stack = stack

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

let dump stack =
  let token_strings =
     (List.map show_token !stack)
     |> List.rev in
  let parts =
     (String.concat " " token_strings)
  in print_endline ( "[ " ^ parts ^ " ]" )

let pop2 stack =
  let a = Stack.pop stack
  and b = Stack.pop stack in
  (a,b)
		   
let apply_prim_op op stack =
  match (pop2 stack) with
  | (INT x, INT y) -> Stack.push stack (INT (op x y))
  | _              -> raise (Failure "Incompatible types for operation")

let add stack  = apply_prim_op (fun x y -> x + y) stack
and sub stack  = apply_prim_op (fun x y -> x - y) stack
and mult stack = apply_prim_op (fun x y -> x * y) stack
and div stack  = apply_prim_op (fun x y -> x / y) stack

let print_forth line = print_endline (line ^ "\nok.")

let dot stack =
  let element = Stack.pop stack in
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
                | _     -> Stack.push stack c)
      tokens
  in stack

let run_proc stack name =
    let procedure = Env.find name in
    let _ = run stack procedure in
    stack

let go program = run (Stack.empty()) program
