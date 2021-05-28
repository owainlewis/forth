(* Main *)

open Forth

let spacer = print_endline "---------------------------------------"

let repl stack =
  let out = Printf.printf "%s\n" in
  let go stack program = 
    let _ = eval stack (parse program) in ()
  in
  try
    while true do
      let line = input_line stdin in
      spacer;
      go stack line;
      spacer;
      out line
    done;
  with UnderflowException _ -> ()

let () =
  spacer;
  print_endline "<< FORTH: A simple forth interpreter >>";
  spacer;
  repl (Stack.empty())