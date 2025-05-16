type vector_unit = 
  | Int of int
  | Float of float

type types = Identifier | BoolT | IntT | FloatT | VectorT of int | MatrixT of int*int | Invalid_type | InputE | InputF;;

exception Variable_Already_Declared;;
exception Variable_Not_Declared;;
(* type checker for AST *)
exception TypeError;;
exception OutOfBoundsError;;
exception Variable_has_no_value;;
(* symbol table for variables - store type names and variables *)
let table : (string,types) Hashtbl.t = Hashtbl.create 10

(* check if the variable is already declared *)
let is_declared vname = Hashtbl.mem table vname

let add_var vname vtype = 
  if Hashtbl.mem table vname then raise Variable_Already_Declared
  else Hashtbl.add table vname vtype

(* check if the variable is of correct type - returns the type of variable *)
let check_var vname = 
  if Hashtbl.mem table vname then 
    Hashtbl.find table vname 
  else raise Variable_Not_Declared

let type_map t = match t with
  | "bool" -> BoolT
  | "int" -> IntT
  | "float" -> FloatT
  | _ -> Invalid_type

let vector_check d = 
  if d<=0 then false
  else true

let matrix_check d1 d2 = 
  if d1<=0 || d2 <= 0 then false
  else true