(* --------------------------------------------- *)
(*            Team name: Super Captain           *)
(* --------------------------------------------- *)
(*            Members information:               *)
(*      Jiangbin Wang  728392  jiangbinw         *)
(*       Xiang Xiang   720138  xxiang2           *)
(*      Yingchen Duan  741032  yingchend         *)
(* --------------------------------------------- *)
 
(* --------------------------------------------------------------- | 
 * Symbol Table                                                    |
 * --------------------------------------------------------------- |
 * Build symbol table for declaration list and the parameter list  |
 * --------------------------------------------------------------- | *)
 
open Snick_ast

(* the size of hash table *)
let hash_table_size = 20
(* a pointer marks the size of an array *)
let add_array = ref(1)
(* a pointer marks the slot number *)
let stack_count = ref (-1)
(* a pointer marks number of main function *)
let main_count = ref (0)
(* create the main hash table *)
let symbol_table_hash = Hashtbl.create hash_table_size

(* get the symbol table for a function *)
let get_hash_table_symbol symbolTableType = match symbolTableType with
    | S_Func(hash_table) -> hash_table
    | _ -> (Printf.printf "Get symbol table for a function failed "; exit 0)

(* add the length of an array *)    
let add_length_of_array interval_list = List.iter(fun x -> (
    match x with
       |Interval(min,max)-> if min<=max && min>=0 && max>=0 
                            then (add_array := !add_array * (max-min+1))
                            else (raise (Failure  "Invalid Array Size. \n") )
       ))interval_list

(* return whether hash table has the id as the key or not *)  
let exist_symbol target_tbl id  =
    Hashtbl.mem target_tbl id

(* add argument list to the hash table; param_name as the key *)
let build_symbol_table_hash_arg_list hash_table arg_list = List.iter(fun x -> ( 
    incr stack_count;
    match x with
        | (Val , snick_type , param_name) -> 
	  let sym = exist_symbol hash_table param_name in 
          if not sym 
          then Hashtbl.add hash_table param_name 
	       (S_Val(Val,snick_type,!stack_count))
          else (raise (Failure  "Arg val exists. \n") )
        | (Ref , snick_type , param_name) -> 
	  let sym = exist_symbol hash_table param_name in
          if not sym 
          then Hashtbl.add hash_table param_name 
	       (S_Ref(Ref,snick_type,!stack_count))
          else (raise (Failure  "Arg ref exists. \n") )
	)) arg_list

(* add declaration list to the hash table; var_name as the key *)
let build_symbol_table_decl_list  hash_table decl_list = List.iter (fun x -> (
    incr stack_count;
    match x with
        | RegDecl(var_name,snick_type) -> 
	  let sym = exist_symbol hash_table var_name in 
          if not sym 
          then Hashtbl.add hash_table var_name 
	       (S_RegDecl(snick_type,!stack_count))
          else (raise (Failure  "Regdecl exists. \n") )
	| ArrayDecl(var_name,snick_type,interval_list) -> 
	  let sym = exist_symbol hash_table var_name in 
          if List.length interval_list = 0 
          then (raise (Failure  "Array has no params. \n") )
          else if not sym then
	  (add_array := 1;
          Hashtbl.add hash_table var_name 
	      (S_ArrayDecl(snick_type,interval_list,!stack_count));
	  add_length_of_array interval_list; 
	  stack_count := !stack_count + !add_array - 1;)
	  else (raise (Failure  "Arraydecl exists. \n") )
	)) decl_list

(* build the main table including the func name as the key and the 
   symbol table of each func as value *)
let build_symbol_type_table procs = List.iter (fun x ->(
    stack_count := -1;
    match x with
        |(func_name,arg_list,(decl_list,_)) ->
	    if func_name == "main" && List.length (arg_list) >0 
	    then (raise (Failure  "Main is not allowed to have parameter. \n"))
	    else if func_name == "main" then main_count := !main_count + 1
	    else if (!main_count)<=1 then
           (Hashtbl.add symbol_table_hash func_name 
	    (S_Func(Hashtbl.create hash_table_size));
            build_symbol_table_hash_arg_list 
	    (get_hash_table_symbol(Hashtbl.find symbol_table_hash func_name)) 
	    arg_list;
            build_symbol_table_decl_list 
	    (get_hash_table_symbol(Hashtbl.find symbol_table_hash func_name)) 
	    decl_list)
	    else (raise (Failure  "Has more than one main function. \n") )
	    )) procs


	

	
	