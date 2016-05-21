open Forth
open MutableStack

let repl input =
  let stack = MutableStack.empty() in
  let result = run stack input in
  (* Eventually we just want to return void here *)
  !result

let main() =
  let rec aux stack =
    let input = read_line() in
    let _ = run stack input in
    aux stack		  
  in
      let stack = MutableStack.empty() in
      let _ = print_endline "<<FORTH: A simple forth interpreter>>" in
  aux stack

let () = main()
