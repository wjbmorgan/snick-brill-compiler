(* --------------------------------------------- *)
(*            Team name: Super Captain           *)
(* --------------------------------------------- *)
(*            Members information:               *)
(*      Jiangbin Wang  728392  jiangbinw         *)
(*       Xiang Xiang   720138  xxiang2           *)
(*      Yingchen Duan  741032  yingchend         *)
(* --------------------------------------------- *)


(* --------------------------------------------------------------- | 
 * Semantic analysis                                               |
 * --------------------------------------------------------------- |
 * Checks the type of ast to ensure there is not type error        |
 * --------------------------------------------------------------- | *)

open Snick_ast
open Snick_symbol
open Snick_codegen

(* Symbol_table is the table that contains all functions *)
let symbol_table = Snick_symbol.symbol_table_hash
(* Get the hash table *)
let get_hash_table = Snick_symbol.get_hash_table_symbol

(* According the ident to find its type using symbol table *)
let get_ident_type procname ident =
  let x =Hashtbl.find(get_hash_table(Hashtbl.find symbol_table procname)) ident 
  in
    match x with
      |S_Val(_, t, _) -> t
      |S_Ref(_, t, _) -> t 
      |S_RegDecl(t, _) -> t 
      |S_ArrayDecl(t, _, _) -> t
      |_ -> failwith "Error."
      
(* Get the type of a lvalue *)
let get_lvalue_type procname lvalue =
  match lvalue with
    |LId(ident) -> get_ident_type procname ident
    |LArray(ident, exprs) -> get_ident_type procname ident

(* If the expr is numeric, get its value *)
let get_num_value expr =
  match expr with
    |Eint(int) -> float_of_int int
    |Efloat(string) -> float_of_string string
    |_ -> 1.0 (* If the expr is not numeric but contains ident, 
                 assume it is not zero for now *)

(* Get the type of a expr *)
let rec get_expr_type procname expr =
  match expr with
    |Ebool(bool) -> Bool
    |Eint(int) -> Int
    |Efloat(string) -> Float
    |Estring(string) -> Str
    |Elval(lvalue) -> get_lvalue_type procname lvalue
    |Ebinop(binopExpr) -> get_binop_type procname binopExpr
    |Eunop(Op_minus, expr1) -> if (get_expr_type procname expr1 = Int || 
                                   get_expr_type procname expr1 = Float) 
			       then get_expr_type procname expr1
                               else 
			       failwith "Error: Operand of unary minus 
			                 must be of numeric type."
    |Eunop(Op_not, expr1) -> if get_expr_type procname expr1 = Bool then Bool
                             else failwith "Error: Operand of not must be bool"

and get_binop_type procname binopExpr =
  match binopExpr with
    |(expr1, Op_and, expr2) -> if (get_expr_type procname expr1 = Bool 
                                  && get_expr_type procname expr2 = Bool) 
				  then Bool
                               else failwith "Error: Arguments of 
			                      logical operator must be bool."
    |(expr1, Op_or, expr2) -> if (get_expr_type procname expr1 = Bool 
                                 && get_expr_type procname expr2 = Bool) 
				 then Bool
                               else failwith "Error: Arguments of 
			                      logical operator must be bool."
    |(expr1, Op_eq, expr2) -> if (get_expr_type procname expr1 = 
                                  get_expr_type procname expr2) then Bool
                               else failwith "Error: Operands of = 
			                      must be have same type."
    |(expr1, Op_noteq, expr2) -> if (get_expr_type procname expr1 = 
                                     get_expr_type procname expr2) then Bool
                               else failwith "Error: Operands of != 
			                      must be have same type."
    |(expr1, Op_lt, expr2) -> if ((get_expr_type procname expr1 = Int || 
                                  get_expr_type procname expr1 = Float) && 
                                  (get_expr_type procname expr2 = Int || 
				  get_expr_type procname expr2 = Float)) 
				  then Bool
                               else failwith "Error: Operands must both have 
			                      numeric type."
    |(expr1, Op_gt, expr2) -> if ((get_expr_type procname expr1 = Int || 
                                   get_expr_type procname expr1 = Float) && 
                                  (get_expr_type procname expr2 = Int || 
				  get_expr_type procname expr2 = Float)) 
				  then Bool
                               else failwith "Error: Operands must both have 
			                      numeric type."
    |(expr1, Op_lteq, expr2) -> if ((get_expr_type procname expr1 = Int || 
                                    get_expr_type procname expr1 = Float) && 
                                    (get_expr_type procname expr2 = Int || 
				    get_expr_type procname expr2 = Float)) 
				    then Bool
                                else failwith "Error: Operands must both have 
				               numeric type."
    |(expr1, Op_gteq, expr2) -> if ((get_expr_type procname expr1 = Int || 
                                    get_expr_type procname expr1 = Float) && 
                                    (get_expr_type procname expr2 = Int || 
				    get_expr_type procname expr2 = Float)) 
				    then Bool
                                else failwith "Error: Operands must both have 
				               numeric type."
    |(expr1, Op_add, expr2) -> if (get_expr_type procname expr1 = Int && 
                                  get_expr_type procname expr2 = Int) then Int
                               else (if ((get_expr_type procname expr1 = Int 
			             && get_expr_type procname expr2 = Float)||
                                     (get_expr_type procname expr1 = Float && 
				     get_expr_type procname expr2 = Int)||
                                     (get_expr_type procname expr1 = Float && 
				     get_expr_type procname expr2 = Float)) 
				     then Float
                                     else failwith "Error: Operands must both 
				                    have numeric type.")
    |(expr1, Op_sub, expr2) -> if (get_expr_type procname expr1 = Int && 
                                   get_expr_type procname expr2 = Int) then Int
                               else (if ((get_expr_type procname expr1 = Int 
			             && get_expr_type procname expr2 = Float)||
                                     (get_expr_type procname expr1 = Float && 
				     get_expr_type procname expr2 = Int)||
                                     (get_expr_type procname expr1 = Float && 
				     get_expr_type procname expr2 = Float)) 
				     then Float
                                     else failwith "Error: Operands must both 
				                    have numeric type.")
    |(expr1, Op_mul, expr2) -> if (get_expr_type procname expr1 = Int && 
                                   get_expr_type procname expr2 = Int) then Int
                               else (if ((get_expr_type procname expr1 = Int 
			             && get_expr_type procname expr2 = Float)||
                                     (get_expr_type procname expr1 = Float && 
				     get_expr_type procname expr2 = Int)||
                                     (get_expr_type procname expr1 = Float && 
				     get_expr_type procname expr2 = Float)) 
				     then Float
                                     else failwith "Error: Operands must both 
				                    have numeric type.")
    |(expr1, Op_div, expr2) -> if (get_expr_type procname expr1 = Int && 
                                   get_expr_type procname expr2 = Int && 
				   (get_num_value expr2 <> 0.0)) then Int
                               else (if ((get_expr_type procname expr1 = Int && 
			                get_expr_type procname expr2 = Float && 
					(get_num_value expr2 <> 0.0))||
                                        (get_expr_type procname expr1 = Float 
					&& get_expr_type procname expr2 = Int 
					&& (get_num_value expr2 <> 0.0))||
                                        (get_expr_type procname expr1 = Float 
					&& get_expr_type procname expr2 = Float 
					&& (get_num_value expr2 <> 0.0))) 
					then Float
                                     else failwith "Error: Operands must both 
				     have numeric type. Expr2 cannot be zero.")
    
(* If an ident is declared array type, get its length *)
let get_array_length procname ident = 
  let x =Hashtbl.find(get_hash_table(Hashtbl.find symbol_table procname)) ident 
  in
  match x with
    |S_ArrayDecl(_, intervals, _) -> List.length intervals
    |_ -> failwith "Error."

(* Check if exprs are all int type *)
let rec check_exprs_is_int procname exprs = 
  match exprs with
  |expr::[] -> get_expr_type procname expr = Int
  |expr::tail -> (get_expr_type procname expr = Int) && 
                 (check_exprs_is_int procname tail)
  |[] -> failwith "Error."

(* Check if an ident is declared array type *)
let check_is_array procname ident =
  let x =Hashtbl.find(get_hash_table(Hashtbl.find symbol_table procname)) ident 
  in
  match x with
    |S_Val(_, _, _) -> false
    |S_Ref(_, _, _) -> false
    |S_RegDecl(_, _) -> false
    |S_ArrayDecl(_, _, _) -> true
    |_ -> failwith "Error."

(* Check if a lvalue is legit *)
let check_lvalue procname lvalue =
  match lvalue with
    |LId(ident) -> let defined = 
                       Hashtbl.mem 
		       (get_hash_table(Hashtbl.find symbol_table procname)) 
		       ident in
                   if not defined then failwith "Error: Symbol not defined.";
                   if (check_is_array procname ident) 
		   then failwith "Error: An array cannot be assigned."
    |LArray(ident, exprs) -> let defined = 
                             Hashtbl.mem 
			   (get_hash_table(Hashtbl.find symbol_table procname)) 
			     ident in
                             if not defined 
			     then failwith "Error: Symbol not defined.";
                             if not (check_is_array procname ident) 
			     then failwith "Error: The type must be array.";
                             if not (check_exprs_is_int procname exprs) 
			     then failwith "Error: expressions must have 
			                    type int.";
                             if not ((List.length exprs) = 
			            (get_array_length procname ident)) 
			     then failwith "Error: length doesn't match 
			                    declaration."

(* Check if a expr is legit *)
let rec check_expr procname expr = 
  match expr with 
    |Elval(lvalue) -> check_lvalue procname lvalue
    |Ebinop((expr1, binop, expr2)) -> check_expr procname expr1;
                                      check_expr procname expr2
    |Eunop((unop, expr)) -> check_expr procname expr
    |_ -> ()

(* Check if a rvalue is legit *)
let check_rvalue procname rvalue = 
  match rvalue with
    |Rexpr(Elval(lvalue)) -> check_lvalue procname lvalue
    |Rexpr(Ebinop((expr1, binop, expr2))) -> check_expr procname expr1;
                                             check_expr procname expr2
    |Rexpr(Eunop((unop, expr))) -> check_expr procname expr
    |_ -> ()

(* Get the type list of actural parameters *)
let rec get_actual_list procname exprs =
  match exprs with
    |[] -> []
    |expr::tail->(get_expr_type procname expr)::(get_actual_list procname tail)

(* Get the type list of a procedure arguments *)
let rec get_type_list procname =
  let tbl = get_hash_table(Hashtbl.find symbol_table procname) in
  let temp = Hashtbl.fold build_type_list tbl [] in
  List.sort comp temp

and build_type_list key value init =
  match value with
    |S_Ref(r, t, s) -> (r, t, s) :: init
    |S_Val(v, t, s) -> (v, t, s) :: init
    |_ -> init
    
and comp (_, _, a) (_, _, b) =
  if a > b then 1 else -1
  
(* Compare two lists to check if types match *)
let rec compare_param_list lista listb =
  if (List.length lista) <> (List.length listb) then false
  else(if (lista = []) && (listb = []) then true
  else (if ((get_inside_type (List.hd lista)) = (List.hd listb)) || 
           (((get_inside_type (List.hd lista)) = Float)&&
	   ((List.hd listb) = Int)&&(get_inside_pass (List.hd lista) = Val)) 
	   then compare_param_list (List.tl lista) (List.tl listb) else false))   

and get_inside_type (_, t, _) = t

and get_inside_pass (p, _, _) = p

(* Check if a statement is legit *)
let rec check_stmt procname stmt = 
  match stmt with
    |Assign(lvalue, rvalue) -> check_assign procname (lvalue, rvalue)
    |Read(lvalue) -> check_read procname lvalue
    |Write(expr) -> check_write procname expr
    |Ifthen(expr, stmts) -> check_ifthen procname (expr, stmts)
    |IfthenElse(expr, stmts1, stmts2) -> 
         check_ifthenelse procname (expr, stmts1, stmts2)
    |WhileDo(expr, stmts) -> check_whiledo procname (expr, stmts)
    |ProcCall(ident, exprs) -> check_proccall procname (ident, exprs)

(* Check if an assign statement is legit *)
and check_assign procname (lvalue, rvalue) =
  check_lvalue procname lvalue;
  check_rvalue procname rvalue;
  let ltype = get_lvalue_type procname lvalue in
  match rvalue with
    |Rexpr(expr) -> let rtype = get_expr_type procname expr in
                    if not (ltype = rtype || (ltype = Float && rtype = Int)) 
		    then
                      failwith "Error: Assignment type mismatch."    

(* Check if a read statement is legit *)
and check_read procname lvalue =
  check_lvalue procname lvalue

(* Check if a write statement is legit *)
and check_write procname expr =
  check_expr procname expr;
  let x = get_expr_type procname expr in
  match x with
    |_ -> ()

(* Check if an ifthen statement is legit *)
and check_ifthen procname (expr, stmts) = 
  if not (get_expr_type procname expr = Bool) 
  then failwith "Error: Condition must be bool.";
  List.iter (check_stmt procname) stmts

(* Check if an ifthenelse statement is legit *)
and check_ifthenelse procname (expr, stmts1, stmts2) = 
  if not (get_expr_type procname expr = Bool) 
  then failwith "Error: Condition must be bool.";
  List.iter (check_stmt procname) stmts1;
  List.iter (check_stmt procname) stmts2

(* Check if a whiledo statement is legit *)
and check_whiledo procname (expr, stmts) = 
  if not (get_expr_type procname expr = Bool) 
  then failwith "Error: Condition must be bool.";
  List.iter (check_stmt procname) stmts

(* Check if a procedure call statement is legit *)
and check_proccall procname (ident, exprs) = 
  let defined = Hashtbl.mem symbol_table_hash ident in
  if not defined then failwith "Error: Procedure not defined.";
  let param_list = get_type_list ident in
  let actual_list = get_actual_list procname exprs in
  if not (compare_param_list param_list actual_list) 
  then failwith "Error: Procedure parameter not match."

(* Check if the procbody is legit *)
let check_proc_body procname (decls, stmts) = 
  List.iter (check_stmt procname) stmts

(* Check if the procedure is legit *)
let check_proc (ident, args, proc_body) =
  check_proc_body ident proc_body

(* Check if the program is legit *)
let check_program prog = 
  List.iter check_proc prog.procs
