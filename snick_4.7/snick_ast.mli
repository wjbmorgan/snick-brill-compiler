(* --------------------------------------------- *)
(*            Team name: Super Captain           *)
(* --------------------------------------------- *)
(*            Members information:               *)
(*      Jiangbin Wang  728392  jiangbinw         *)
(*       Xiang Xiang   720138  xxiang2           *)
(*      Yingchen Duan  741032  yingchend         *)
(* --------------------------------------------- *)

(* ----------------------------------------------------- | 
 * Abstract Syntax Tree for Snick language               |
 * ----------------------------------------------------- |
 * Tree representation of Snick program in program       |
 * built by the Snick parser                             |
 * ----------------------------------------------------- | *)

type ident = string
 
(* Keep aliases intact for pretty printing. *)
type snicktype =
  | Bool | Int | Float | Str

type arg_pass_type = 
  | Val | Ref

type binop =
  | Op_add | Op_sub | Op_mul | Op_div | Op_eq | Op_lt | Op_gt | Op_noteq 
  | Op_gteq | Op_lteq | Op_and | Op_or

type unop =
  | Op_minus
  | Op_not

(* Mutually recursive types expr and lvalue *)
type expr =
  | Ebool of bool
  | Eint of int
  | Efloat of string
  | Estring of string
  | Elval of lvalue
  | Ebinop of binopExpr
  | Eunop of unopExpr
and lvalue =
  | LId of ident
  | LArray of (ident * expr list)
and binopExpr = (expr * binop * expr)
and unopExpr = (unop * expr)

(* Will need to AST elements with additional data.  *)
type rvalue =
  | Rexpr of expr

(* First int is lower bound, Second int is upper bound *)
type interval = 
  | Interval of (int * int)

type decl = 
  | RegDecl of (ident * snicktype)
  | ArrayDecl of (ident * snicktype * interval list)


type stmt = 
  | Assign of (lvalue * rvalue)
  | Read of lvalue
  | Write of expr
  | Ifthen of (expr * stmt list)
  | IfthenElse of (expr * stmt list * stmt list)
  | WhileDo of (expr * stmt list)
  | ProcCall of (ident * expr list)

type arg = (arg_pass_type * snicktype * ident)

type proc_body = (decl list * stmt list)

type stackNum = int

type symbolTableType =
    | S_Func of (ident , symbolTableType) Hashtbl.t
    | S_Ref of (arg_pass_type * snicktype * stackNum)
    | S_Val of (arg_pass_type * snicktype * stackNum)
    | S_RegDecl of (snicktype * stackNum)
    | S_ArrayDecl of (snicktype * interval list * stackNum)

type proc = (ident * arg list * proc_body)

type program = {
  procs : proc list
}
 
type t = program
