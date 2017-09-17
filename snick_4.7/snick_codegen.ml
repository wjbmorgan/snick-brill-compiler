(* --------------------------------------------- *)
(*            Team name: Super Captain           *)
(* --------------------------------------------- *)
(*            Members information:               *)
(*      Jiangbin Wang  728392  jiangbinw         *)
(*       Xiang Xiang   720138  xxiang2           *)
(*      Yingchen Duan  741032  yingchend         *)
(* --------------------------------------------- *)


(* --------------------------------------------------------------- | 
 * Code generation                                                 |
 * --------------------------------------------------------------- |
 * Genenrates grill context                                        |
 * --------------------------------------------------------------- | *)

open Snick_ast
open Snick_symbol

(* a pointer points the number of register that will be used*)
let cur_register_count = ref 0

(* a pointer marks the number of label number that will be used *)
let cur_label_count =ref (-1)

(* a pointer used to calculate the size of stack number allocated *)
let size = ref 0

(* a pointer records the type of expression now used *)
let expr_type = ref Str

(* get the symbol table from Snick_symbol *)
let symbolt = Snick_symbol.symbol_table_hash

(* get the string name of register number *)
let get_r_str registerN = "r"^(string_of_int registerN)

(* stack operation *)
let print_push_stack_frame f_size = Printf.printf "push_stack_frame %d\n" f_size
let print_pop_stack_frame f_size = Printf.printf "pop_stack_frame %d\n" f_size

(* load or store given number to named register *)
let print_load r slotN = Printf.printf "load %s, %d\n" r slotN
let print_store slotN r = Printf.printf "store %d, %s\n" slotN r
let print_load_address r slotN = Printf.printf "load_address %s, %d\n" r slotN
let print_load_indirect rI rJ = Printf.printf "load_indirect %s, %s\n" rI rJ
let print_store_indirect rI rJ = Printf.printf "store_indirect %s, %s\n" rI rJ

(* load a constant of the specified type to the named register *)
let print_int_const r int = Printf.printf "int_const %s, %d\n" r int
let print_real_const r real = Printf.printf "real_const %s, %s\n" r real
let print_string_const r string = Printf.printf "string_const %s, %s\n" r string

(* operation *)
let print_add_int rI rJ rK = Printf.printf "add_int %s, %s, %s\n" rI rJ rK
let print_add_real rI rJ rK = Printf.printf "add_real %s, %s, %s\n" rI rJ rK
let print_add_offset rI rJ rK = Printf.printf "add_offset %s, %s, %s\n" rI rJ rK
let print_sub_int rI rJ rK = Printf.printf "sub_int %s, %s, %s\n" rI rJ rK
let print_sub_real rI rJ rK = Printf.printf "sub_real %s, %s, %s\n" rI rJ rK
let print_sub_offset rI rJ rK = Printf.printf "sub_offset %s, %s, %s\n" rI rJ rK
let print_mul_int rI rJ rK = Printf.printf "mul_int %s, %s, %s\n" rI rJ rK
let print_mul_real rI rJ rK = Printf.printf "mul_real %s, %s, %s\n" rI rJ rK
let print_div_int rI rJ rK = Printf.printf "div_int %s, %s, %s\n" rI rJ rK
let print_div_real rI rJ rK = Printf.printf "div_real %s, %s, %s\n" rI rJ rK

(* comparisons *)
let print_cmp_eq_int rI rJ rK = Printf.printf "cmp_eq_int %s, %s, %s\n" rI rJ rK
let print_cmp_ne_int rI rJ rK = Printf.printf "cmp_ne_int %s, %s, %s\n" rI rJ rK
let print_cmp_gt_int rI rJ rK = Printf.printf "cmp_gt_int %s, %s, %s\n" rI rJ rK
let print_cmp_ge_int rI rJ rK = Printf.printf "cmp_ge_int %s, %s, %s\n" rI rJ rK
let print_cmp_lt_int rI rJ rK = Printf.printf "cmp_lt_int %s, %s, %s\n" rI rJ rK
let print_cmp_le_int rI rJ rK = Printf.printf "cmp_le_int %s, %s, %s\n" rI rJ rK
let print_cmp_eq_real rI rJ rK=Printf.printf "cmp_eq_real %s, %s, %s\n" rI rJ rK
let print_cmp_ne_real rI rJ rK=Printf.printf "cmp_ne_real %s, %s, %s\n" rI rJ rK
let print_cmp_gt_real rI rJ rK=Printf.printf "cmp_gt_real %s, %s, %s\n" rI rJ rK
let print_cmp_ge_real rI rJ rK=Printf.printf "cmp_ge_real %s, %s, %s\n" rI rJ rK
let print_cmp_lt_real rI rJ rK=Printf.printf "cmp_lt_real %s, %s, %s\n" rI rJ rK
let print_cmp_le_real rI rJ rK=Printf.printf "cmp_le_real %s, %s, %s\n" rI rJ rK

(* logical operation *)
let print_and rI rJ rK = Printf.printf "and %s, %s, %s\n" rI rJ rK
let print_or rI rJ rK = Printf.printf "or %s, %s, %s\n" rI rJ rK
let print_not rI rJ = Printf.printf "not %s, %s\n" rI rJ

(* convert the integer in the source register to a real number *)
let print_int_to_real rI rJ = Printf.printf "int_to_real %s, %s\n" rI rJ

(* copy value from one register to another, input are registers *)
let print_move rI rJ = Printf.printf "move %s, %s\n" rI rJ

(* branch operation used in comparisons, input are register and label_name *)
let print_branch_on_true r l = Printf.printf "branch_on_true %s, %s\n" r l
let print_branch_on_false r l = Printf.printf "branch_on_false %s, %s\n" r l
let print_branch_uncond l = Printf.printf "branch_uncond %s\n" l

(* call related operation *)
let print_call label = Printf.printf "call %s\n" label 
let print_call_builtin builtin_funciton_name = 
	    Printf.printf "call_builtin %s\n" builtin_funciton_name
let print_return () = Printf.printf "return\n"

(* debug *)
let print_debug_reg register = Printf.printf "debug_reg %s\n" register
let print_debug_slot slotN = Printf.printf "debug_slot %d\n" slotN
let print_debug_stack () = Printf.printf "debug_stack\n"

(* halt *)
let print_halt () = Printf.printf "halt\n"

(* built-in function *)
let print_read_int () = print_call_builtin "read_int"
let print_read_bool () = print_call_builtin "read_bool"
let print_read_real () = print_call_builtin "read_real"
let print_print_int () = print_call_builtin "print_int"
let print_print_bool () = print_call_builtin "print_bool"
let print_print_real () = print_call_builtin "print_real"
let print_print_string () = print_call_builtin "print_string"

(* print the program *)
let rec print_program prog =
	let procs = prog.procs
    in
    print_procs procs symbolt

(* check if program is empty, if not, call proc_main first*)
and print_procs procs symboltable =
	match procs with
    | [] -> ()
    | _  -> print_call "proc_main"; 
            print_halt (); 
            print_proc_list procs symboltable

(* print procs' label *)
and print_proc_list procs symboltable =
	match procs with
	| [] -> ()
	| (id, args, proc_body) :: ps ->  
	        Printf.printf "proc_%s:\n" id;
	        let symboltype = Hashtbl.find symboltable id
	        in
	        let func_table = Snick_symbol.get_hash_table_symbol symboltype
	        in
	        print_body args proc_body func_table; 
	        print_proc_list ps symboltable

(* print prologue, push_stack_frame and pop_stack_frame and epilogue *)
and print_body args (decls, stmts) func_table =
	Printf.printf "# prologue\n";
	cal_length func_table;
	let framesize = !size in
	if framesize > 0 then print_push_stack_frame framesize;
	cur_register_count := 0;
	print_prologue_args args func_table;
	print_prologue_proc decls func_table;
	print_stmt stmts func_table;
	Printf.printf "# epilogue\n";
	if framesize > 0 then print_pop_stack_frame framesize;
	print_return ();
	size := 0

(* calculate the size of stack needed *)
and cal_length func_table = 
    Hashtbl.iter (fun key value ->
    match value with
    | S_ArrayDecl(_, intervals, _) -> 
          size := !size + (get_length_of_array intervals 1)
    | _ -> size := !size + 1) func_table

(* get the length of array *)
and get_length_of_array intervals count =
    match intervals with
    | [] -> count
    | Interval(min, max) :: rest -> 
          get_length_of_array rest (count * (max - min + 1))

    
and print_snicktype snicktype =
    match snicktype with
    | Bool -> "bool"
    | Int -> "int"
    | Float -> "real" 
    | Str -> "string"   
    

(* print prologue of arguments *)
and print_prologue_args args func_table = 
	match args with
	| [] -> ()
	| (arg_pass_type, snicktype, id) :: arguments -> 
	      Printf.printf "# %s %s\n" (print_snicktype snicktype) id; 
	      let argu = Hashtbl.find func_table id in
          let local_r = !cur_register_count in
        (match argu with
        | S_Val(_,_,slotN) -> print_store slotN (get_r_str local_r);
                              cur_register_count := !cur_register_count + 1;
	                          print_prologue_args arguments func_table;
        | S_Ref(_,_,slotN) -> print_store slotN (get_r_str local_r);
                              cur_register_count := !cur_register_count + 1;
	                          print_prologue_args arguments func_table		
        | _ -> (raise (Failure "print_prologue_args, type error\n")))					                                         
	      

(* print prologue of assigments *)
and print_prologue_proc decls func_table =
	cur_register_count := 0;
	match decls with
	| [] -> ()
	| RegDecl(id, snicktype) :: ds -> 
	      Printf.printf "# %s %s\n" (print_snicktype snicktype) id;
	      print_init snicktype (get_r_str !cur_register_count);
	      let value = Hashtbl.find func_table id 
	      in
        (match value with
          | S_RegDecl(_,slotnum) ->
	            print_store slotnum (get_r_str !cur_register_count);
	            print_prologue_proc ds func_table
          | _ -> (raise (Failure "print_prologue_proc, type error\n")))
	| ArrayDecl(id, snicktype, interval) :: ds -> 
	      Printf.printf "# %s %s\n" (print_snicktype snicktype) id;
	      print_init snicktype (get_r_str !cur_register_count);
	      let value = Hashtbl.find func_table id 
	      in
        (match value with
          | S_ArrayDecl(_,_,slotnum) ->
	            print_init_array slotnum (get_length_of_array interval 1);
	            print_prologue_proc ds func_table
          | _ -> (raise (Failure "print_prologue_proc, array type error\n")))

(* print prologue of array *)
and print_init_array slotnum length =
	match length with
	| 0 -> ()
	| _ -> print_store slotnum (get_r_str !cur_register_count);
	       print_init_array (slotnum+1) (length-1)

(* initialize the id with snicktype *)
and print_init snicktype register =
	match snicktype with
	| Int -> print_int_const register 0
	| Float -> print_real_const register "0.0"
	| Bool -> print_int_const register 0
  | _ -> (raise (Failure "type error when initialize variables\n") )

(* print statements *)
and print_stmt stmts func_table =
	match stmts with
	| [] -> ()
	| stmt :: sts -> print_one_stmt func_table stmt; print_stmt sts func_table 

(* print one statement *)
and print_one_stmt func_table stmt = 
	match stmt with
    | Assign(lvalue, rvalue) -> 
          Printf.printf "# assigment\n";
          cur_register_count := 0;
          let Rexpr(expr) = rvalue
          in
          let local_r = !cur_register_count
          in
          print_arithmatic func_table expr;
          cur_register_count := !cur_register_count - 1;
          let temp_lvalue_type = get_lvalue_type func_table lvalue 
          in
          let temp_rvalue_type = !expr_type 
          in         
          if (temp_lvalue_type = Float) && (temp_rvalue_type = Int) then
                print_int_to_real (get_r_str local_r) 
                                  (get_r_str local_r);
          print_store_lvalue lvalue func_table   

    | Read(lvalue) -> Printf.printf "# read\n";
                      cur_register_count := 0;
                      let temp_lvalue_type = (get_lvalue_type func_table lvalue) 
                      in 
		                  let str = print_snicktype temp_lvalue_type
		                  in
                      print_call_builtin ("read_"^str);
                      print_store_lvalue lvalue func_table

    | Write(expr) -> Printf.printf "# write\n";
                     cur_register_count := 0;
                     print_arithmatic func_table expr;
                     cur_register_count := !cur_register_count - 1;
                     let temp_rvalue_type = !expr_type
                     in
                     print_call_builtin 
                         ("print_"^(print_snicktype temp_rvalue_type));

    | Ifthen (expr,then_stmt_list) -> 
          Printf.printf "# if\n";
          cur_register_count := 0;
          print_arithmatic func_table expr;
          cur_label_count := (!cur_label_count) + 1;
          let label = "label_" ^ (string_of_int !cur_label_count) 
          in
          cur_register_count := 0;
          print_branch_on_false (get_r_str !cur_register_count) label;
          List.iter (print_one_stmt func_table) then_stmt_list;
          Printf.printf "%s:\n" label
    
    | IfthenElse (expr,then_stmt_list,else_stmt_list) ->
          Printf.printf "# if\n";
          cur_register_count := 0;
          print_arithmatic func_table expr;
          cur_label_count := (!cur_label_count) + 1;
          let label = "label_" ^ (string_of_int !cur_label_count)
          in
          cur_register_count := 0;
          print_branch_on_false (get_r_str !cur_register_count) label;
          List.iter (print_one_stmt func_table) then_stmt_list;
          cur_label_count := (!cur_label_count) + 1;
          let label2 = "label_" ^ (string_of_int !cur_label_count) 
          in
          print_branch_uncond label2;
          Printf.printf "%s:\n" label;
          Printf.printf "# else\n";
          List.iter (print_one_stmt func_table) else_stmt_list;
          Printf.printf "%s:\n" label2;

    | WhileDo(expr, stmt_list) ->  
          Printf.printf "# while\n";
          cur_label_count := (!cur_label_count) + 2;
          let label = "label_" ^ (string_of_int (!cur_label_count - 1)) 
          in
          let label2 = "label_" ^ (string_of_int !cur_label_count)  
          in
          Printf.printf "%s:\n" label;
          cur_register_count := 0;
          print_arithmatic func_table expr;
          cur_register_count := 0;
          print_branch_on_false (get_r_str !cur_register_count) label2;
          List.iter (print_one_stmt func_table) stmt_list;
          print_branch_uncond label;
          Printf.printf "%s:\n" label2;    

    | ProcCall(id, expr_list) -> 
          Printf.printf "# call %s\n" id;
          cur_register_count := 0;
          let symboltype = Hashtbl.find symbolt id
          in
          let call_table = Snick_symbol.get_hash_table_symbol symboltype
	        in
          List.iter (print_proc_call call_table func_table) expr_list;
          let symbol = "proc_" ^ id 
          in
          print_call symbol


(* print proc_call *)
and print_proc_call call_table func_table expr = 
        let local_r = !cur_register_count in
        Hashtbl.iter (fun key value ->
	      match value with
	      | S_Val(arg_type,snicktype,slotnum) -> 
            if (slotnum = local_r) then
	          (print_arithmatic func_table expr;
             if snicktype = Float && !expr_type = Int then
             print_int_to_real (get_r_str local_r) (get_r_str local_r))
        | S_Ref(arg_type,_,slotnum)	-> 
            if (slotnum = local_r) then
	             (print_load_address (get_r_str local_r) 
	                               (get_lvalue_slotnum func_table expr);
              cur_register_count := !cur_register_count + 1)
        | _ -> () 
	      ) call_table
        


(* print lvalue *)
and print_store_lvalue lvalue func_table =
    match lvalue with
    | LId(id) -> 
          let value = Hashtbl.find func_table id
          in
          (match value with
          | S_Val(Val,_,slotnum) -> print_store slotnum 
                                                (get_r_str !cur_register_count)
          | S_Ref(Ref,_,slotnum) -> 
                  let local_r = !cur_register_count
                  in
                  print_load (get_r_str (local_r + 1)) slotnum;           
                  print_store_indirect (get_r_str (local_r + 1)) 
                                       (get_r_str local_r)
          | S_RegDecl(_,slotnum) -> print_store slotnum 
                                                (get_r_str !cur_register_count)
          | _ -> (raise (Failure "print_store_lvalue error, error type\n")))  
		 
    | LArray(id, exprs) -> 
          let argu = Hashtbl.find func_table id
          in
		  (match argu with
		  | S_ArrayDecl(_,intervals,slotnum) ->
                let local_r = !cur_register_count
                in
                calculate_array_num func_table intervals exprs (local_r+1);
                print_load_address (get_r_str (local_r + 2)) slotnum;
                print_sub_offset (get_r_str (local_r + 1))
                                 (get_r_str (local_r + 2))
                                 (get_r_str (local_r + 1));
                print_store_indirect (get_r_str (local_r + 1)) 
                                     (get_r_str (local_r))
      | _ -> (raise (Failure "print_store_lvalue error, error type\n")))
			       

(* get type of lvalue *)
and get_lvalue_type func_table lvalue =
	match lvalue with
	| LId(id) -> let value = Hashtbl.find func_table id in
	             (match value with
	             | S_Val(Val,arg_type,_) -> arg_type
	             | S_Ref(Ref,arg_type,_) -> arg_type
	             | S_RegDecl(arg_type,_) -> arg_type
               | _ -> (raise (Failure "get_lvalue_type, error type\n")))
	| LArray(id,_) -> let value = Hashtbl.find func_table id in
	                  (match value with
	                  | S_ArrayDecl(arg_type,_,_) -> arg_type
                    | _ -> (raise (Failure "get_lvalue_type, error array\n"))) 

(* get slotnum of lvalue *)
and get_lvalue_slotnum func_table expr =
  match expr with
  | Elval(LId(id)) -> let value = Hashtbl.find func_table id in
                    (match value with
                    | S_Val(Val,arg_type,slotnum) -> slotnum
                    | S_Ref(Ref,arg_type,slotnum) -> slotnum
                    | S_RegDecl(arg_type,slotnum) -> slotnum
                    | _ -> (raise (Failure "get_lvalue_slotnum, error type\n")))
  | Elval(LArray(id,exprs)) -> 
      let value = Hashtbl.find func_table id in
      (match value with
      | S_ArrayDecl(arg_type,intervals,slotnum) ->
            let list_ = List.combine intervals exprs in
            get_array_num list_ slotnum 0
      | _ -> (raise (Failure "get_lvalue_slotnum, error array\n"))) 
  | _ -> (raise (Failure "get_lvalue_slotnum, error ref id\n"))

(* calculate the number of array *)
and get_array_num list_ slotnum count =
      match list_ with 
      | [] -> (count + slotnum)
      | (Interval(lo, hi), Eint(i)) :: lists ->
           get_array_num lists slotnum (count * (hi - lo + 1) + (i - lo))
      | _ -> (raise (Failure "get_array_slotnum, error type\n"))


(* print expression and store the type of expression *)
and print_arithmatic func_table expr = 
	let local_r = !cur_register_count in 
	match expr with
    | Ebool(false) -> print_int_const (get_r_str (local_r)) 0;
                      incr cur_register_count;
                      expr_type := Bool

    | Ebool(true) -> print_int_const (get_r_str (local_r)) 1;
                     incr cur_register_count;
                     expr_type := Bool

    | Eint(int_val) -> print_int_const (get_r_str (local_r)) int_val;
                       incr cur_register_count;
                       expr_type := Int

    | Efloat(float_val) -> print_real_const (get_r_str (local_r)) float_val;
                         incr cur_register_count;
                         expr_type := Float

    | Estring(string_val) -> print_string_const (get_r_str (local_r)) string_val;
                             incr cur_register_count;
                             expr_type := Str 

    | Elval(lvalue) -> 
          incr cur_register_count;
          (match lvalue with
          | LId(id) -> 
               let value = Hashtbl.find func_table id in
	           (match value with
	           | S_Val(Val,arg_type,slotnum) -> print_load (get_r_str (local_r)) 
	                                                       slotnum;
	                                            expr_type := arg_type
	           | S_Ref(Ref,arg_type,slotnum) -> print_load (get_r_str (local_r)) 
	                                                       slotnum;
	                                            print_load_indirect 
	                                                      (get_r_str (local_r))
	                                                      (get_r_str (local_r));
	                                            expr_type := arg_type
	           | S_RegDecl(arg_type,slotnum) -> print_load (get_r_str (local_r)) 
	                                                       slotnum;
	                                            expr_type := arg_type
             | _ -> (raise (Failure "print_arithmatic, error type LId\n")))
          | LArray(id,exprs) -> 
             let value = Hashtbl.find func_table id in
	           (match value with
	           | S_ArrayDecl(arg_type,intervals,slotnum) -> 
	                let r = get_r_str (local_r) 
	                in
	                let r1 = get_r_str (local_r + 1)
	                in
	                calculate_array_num func_table intervals exprs local_r;
	                print_load_address r1 slotnum;
	                print_sub_offset r r1 r;
	                print_load_indirect r r;
	                expr_type := arg_type
              | _ -> (raise (Failure "print_arithmatic, error type LArray\n"))))
	                                                                     
    | Ebinop(expr_one,binop,expr_two) -> 
          print_arithmatic func_table expr_one;
          let type1 = !expr_type in
          print_arithmatic func_table expr_two;
          let type2 = !expr_type in
          decr cur_register_count; (*decrease register used*)
          (match (type1, type2) with
          | (Int, Float) -> print_int_to_real (get_r_str local_r) 
                                              (get_r_str local_r);
                            print_binop (get_r_str (local_r)) 
                                        (get_r_str (local_r)) 
                                        (get_r_str (local_r + 1)) 
                                        type2
                                        binop
          | (Float, Int) ->   print_int_to_real (get_r_str (local_r+1)) 
                                              (get_r_str (local_r+1));
                              print_binop (get_r_str (local_r)) 
                              (get_r_str (local_r)) 
                              (get_r_str (local_r + 1)) 
                              type1
                              binop 
          | _ ->  print_binop (get_r_str (local_r)) (get_r_str (local_r)) 
                              (get_r_str (local_r + 1)) type1 binop)               
                                         
    | Eunop(Op_minus,expr) -> print_arithmatic func_table expr;
                              print_int_const (get_r_str (local_r+1)) (-1);
                              print_mul_int (get_r_str local_r) 
                                            (get_r_str local_r) 
                                            (get_r_str (local_r+1))

    | Eunop(Op_not,expr) ->  print_arithmatic func_table expr;
                             print_not (get_r_str (local_r)) 
                                       (get_r_str (local_r));
                             expr_type := Bool

(* print binop and store the type of expression*)
and print_binop rI rI rJ type_op binop =
    match (type_op, binop) with
    | (Int, Op_add) -> print_add_int rI rI rJ; expr_type := type_op
    | (Int, Op_sub) -> print_sub_int rI rI rJ; expr_type := type_op
    | (Int, Op_mul) -> print_mul_int rI rI rJ; expr_type := type_op
    | (Int, Op_div) -> print_div_int rI rI rJ; expr_type := type_op
    | (Int, Op_eq) ->  print_cmp_eq_int rI rI rJ; expr_type := Bool
    | (Int, Op_lt) ->  print_cmp_lt_int rI rI rJ; expr_type := Bool
    | (Int, Op_gt) ->  print_cmp_gt_int rI rI rJ; expr_type := Bool
    | (Int, Op_noteq) -> print_cmp_ne_int rI rI rJ; expr_type := Bool
    | (Int, Op_lteq) -> print_cmp_le_int rI rI rJ; expr_type := Bool
    | (Int, Op_gteq) -> print_cmp_ge_int rI rI rJ; expr_type := Bool

    | (Float, Op_add) -> print_add_real rI rI rJ; expr_type := type_op
    | (Float, Op_sub) -> print_sub_real rI rI rJ; expr_type := type_op
    | (Float, Op_mul) -> print_mul_real rI rI rJ; expr_type := type_op
    | (Float, Op_div) -> print_div_real rI rI rJ; expr_type := type_op
    | (Float, Op_eq) ->  print_cmp_eq_real rI rI rJ; expr_type := Bool
    | (Float, Op_lt) ->  print_cmp_lt_real rI rI rJ; expr_type := Bool
    | (Float, Op_gt) ->  print_cmp_gt_real rI rI rJ; expr_type := Bool
    | (Float, Op_noteq) -> print_cmp_ne_real rI rI rJ; expr_type := Bool
    | (Float, Op_lteq) -> print_cmp_le_real rI rI rJ; expr_type := Bool
    | (Float, Op_gteq) -> print_cmp_ge_real rI rI rJ; expr_type := Bool

    | (Bool, Op_eq) ->  print_cmp_eq_int rI rI rJ; expr_type := Bool
    | (Bool, Op_lt) ->  print_cmp_lt_int rI rI rJ; expr_type := Bool
    | (Bool, Op_gt) ->  print_cmp_gt_int rI rI rJ; expr_type := Bool
    | (Bool, Op_noteq) -> print_cmp_ne_int rI rI rJ; expr_type := Bool
    | (Bool, Op_lteq) -> print_cmp_le_int rI rI rJ; expr_type := Bool
    | (Bool, Op_gteq) -> print_cmp_ge_int rI rI rJ; expr_type := Bool

    | (_,Op_and) -> print_and rI rI rJ; expr_type := Bool
    | (_,Op_or) ->  print_or rI rI rJ; expr_type := Bool

    | _ -> (raise (Failure "print_binop, error binop type\n"))

(* calculate the number of element in array
   the formula like (((((i − lo1) ∗ s2) + (j − lo2)) ∗ s3) + (k − lo3))
   s2 = hi2 − lo2 + 1 and s3 = hi3 − lo3 + 1
   we find that s1 is not be used, so we calculate head of intervals alone *)
and calculate_array_num func_table intervals exprs local_r = 
    rec_calculate_array_num_head func_table intervals exprs local_r;
    let list_ = List.combine (List.tl intervals) (List.tl exprs) 
    in
    rec_calculate_array_num func_table list_ local_r;

and rec_calculate_array_num_head func_table intervals exprs local_r =
    let expr = List.hd exprs in
    let Interval(lo, hi) = List.hd intervals in
    match expr with
    | Eint(i) -> print_int_const (get_r_str local_r) i;
                 print_int_const (get_r_str (local_r+1)) lo;
                 print_sub_int (get_r_str local_r) (get_r_str local_r) 
                               (get_r_str (local_r+1));
    | _ -> cur_register_count := local_r;
           print_arithmatic func_table expr;
           cur_register_count := !cur_register_count - 1;
           print_int_const (get_r_str (local_r+1)) lo;
           print_sub_int (get_r_str local_r) (get_r_str local_r) 
                               (get_r_str (local_r+1));
    

and rec_calculate_array_num func_table list_ local_r =       
    match list_ with
    | [] -> ()
    | (Interval(lo,hi),expr) :: lists -> 
          print_int_const (get_r_str (local_r+1)) hi;
          print_int_const (get_r_str (local_r+2)) lo;
          print_sub_int (get_r_str (local_r+1)) (get_r_str (local_r+1))
                        (get_r_str (local_r+2));
          print_int_const (get_r_str (local_r+2)) 1;
          print_add_int (get_r_str (local_r+1)) (get_r_str (local_r+1)) 
                        (get_r_str (local_r+2));
          print_mul_int (get_r_str (local_r)) (get_r_str (local_r)) 
                        (get_r_str (local_r+1));
          cur_register_count := local_r + 1;
          print_arithmatic func_table expr;
          cur_register_count := !cur_register_count - 1;
          print_int_const (get_r_str (local_r+2)) lo;
          print_sub_int (get_r_str (local_r+1)) (get_r_str (local_r+1)) 
                        (get_r_str (local_r+2));
          print_add_int (get_r_str (local_r)) (get_r_str (local_r)) 
                        (get_r_str (local_r+1));
          rec_calculate_array_num func_table lists local_r