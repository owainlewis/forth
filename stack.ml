module type MUTABLE_STACK =
  sig
    type 'a mstack
    val empty : unit -> 'a mstack
    val push : 'a mstack -> 'a -> unit
    val pop : 'a mstack -> 'a option
  end

module S : MUTABLE_STACK =
  struct
    type 'a mstack = ('a list) ref
    let empty () : 'a mstack = ref []
    let push (s: 'a mstack) x = s := x :: (!s)
    let pop s =
      match (!s) with
      | [] -> None
      | x::xs -> let _ = s := xs in Some x
end

module MutableStack = struct
  
  type 'a stack = ('a list) ref
			     
  let empty () = ref []

  let peek stack =
    match !stack with | [] -> None | x :: _ -> Some(x)

  let push stack value =
    let new_stack = (value :: !stack) in
    stack := new_stack

  let pop stack =
    match (!stack) with
    | []    -> raise (Failure "Empty stack")
    | x::xs ->
       let _ = stack := xs in
       x

  let inspect stack = !stack
end
