module Env = struct
  
  let env = ref [ ]
		  
  let insert k v =
    let existing_env = !e in
    env := (k, v) :: existing_env
				   
  let find p = List.assoc p !env
end
