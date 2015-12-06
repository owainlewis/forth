(*****************************************************************)
(* Stack implementation *)
(*****************************************************************)

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
  
(*****************************************************************)
