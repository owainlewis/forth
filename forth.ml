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
  | x   -> ATOM x

(* Lex the input into actionable tokens *)
let lex input =
  let rec aux lexed = function
      []      -> lexed
    | (x::xs) -> aux (lex_token x :: lexed) xs
  in let lex_result = aux [] (tokenize input) in
  List.rev lex_result

(* Apply the + function to our stack *)
let add stack =
  let a = Stack.pop stack
  and b = Stack.pop stack in
  let result = (int_of_string a) + (int_of_string b) in
  Stack.push (string_of_int result) stack

let run stack input =
  let tokens = tokenize input in
  let _ = List.iter (fun c ->
              if c = "+" then
                add(stack)
              else Stack.push c stack) tokens
  in stack

let repl input =
  let stack = Stack.create() in
  run stack input
