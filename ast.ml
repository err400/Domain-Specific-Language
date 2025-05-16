open Types

type exp = 
  (* string variable with its value *)
  (* | Comment *)
  | String_var of string
  | Bool_var of bool
  | Int_var of int
  | Float_var of float
  (* handle dimensions also *)
  | Vector of vector_unit list
  | Matrix of vector_unit list list
  | Not of exp
  | And of exp*exp
  | Or of exp*exp
  (* for floats and ints *)
  | Add of exp*exp
  | Mul of exp*exp
  | Sub of exp*exp
  | Div of exp*exp
  | Abs of exp
  | Sqrt of exp
  | Power of exp*exp
  (* ints - equality and comparisons and remainder *)
  | Is_equal of exp*exp
  | Is_greater of exp*exp
  | Is_lesser of exp*exp
  | Is_less_than_equal of exp*exp
  | Is_greater_than_equal of exp*exp
  | Rem of exp*exp 
  | Not_equal_to of exp*exp
  | Scal_prod of exp*exp
  | Dot_prod of exp*exp
  | Angle of exp*exp
  | Mag of exp
  | Dim of exp
  | Matrix_add of exp*exp
  | Matrix_mul of exp*exp
  | Matrix_transpose of exp
  | Matrix_determinant of exp
  | Matrix_row of exp
  | Matrix_col of exp
  (* | Matrix_mag of exp *)
  | Matrix_minor of exp*exp*exp
  | Matrix_inverse of exp
  | Input
  | InputFile of string
  | Create_empty_matrix of int*int
  | Vector_access of string*exp
  | Matrix_access of string*exp*exp

(* control constructs *)
type control = 
  (* | Assign of string*exp *)
  | Assign of string*exp
  | Matrix_assign of exp*exp
  | Declare of string*types
  | Seq of control list
  | If_then_else of exp*control list*control list
  (* variables, loop start, loop end , sequence *)
  | For of string*exp*exp*control list
  | While of exp*control list
  | Print of string

let rec dim_of_vector v = match v with
| [] -> 0
| h::t -> 1 + (dim_of_vector t)

let rec check_all m n = match m with
  | [] -> true
  | h :: t -> if (dim_of_vector h) = n then (check_all t n)
      else false

let rec dim_of_matrix m = match m with
  | [] -> (0,0)
  | h::t -> if (check_all m (dim_of_vector h)) then (dim_of_vector m, dim_of_vector h)
            else raise TypeError

let rec type_of_vector v = match v with
  | [] -> raise TypeError
  | (Int _:: []) -> IntT
  | (Float _:: []) -> FloatT
  | Int _:: t -> (match type_of_vector t with 
    | IntT -> IntT
    | _ -> raise TypeError)
  | Float _ :: t -> (match type_of_vector t with 
    | FloatT -> FloatT
    | _ -> raise TypeError)

type start_symbol =
(* | Exp of exp *)
| Control of control list

let rec type_of e = match e with
  | String_var e -> check_var e
  | Bool_var _ -> BoolT
  | Int_var _ -> IntT
  | Float_var _ -> FloatT
  | Vector v -> 
    let d = dim_of_vector v in (VectorT d)
  | Matrix m -> 
    let (x,y) = dim_of_matrix m in MatrixT (x, y)
  | Not e1 -> if type_of e1 = BoolT then BoolT
    else raise TypeError
  | And(e1,e2) -> 
    if type_of e1 = BoolT && type_of e2 = BoolT then BoolT
    else raise TypeError
  | Or(e1,e2) -> 
    if type_of e1 = BoolT && type_of e2 = BoolT then BoolT
    else raise TypeError
  (* int/float, vector, matrix *)
  | Add(e1,e2) -> (match (type_of e1, type_of e2) with 
    | (IntT, IntT) -> IntT
    | (FloatT, FloatT) -> FloatT
    (* check dimensions in case of vector/matrix *)
    | (VectorT n1, VectorT n2) -> 
      if n1 = n2 then VectorT n1
      else raise TypeError
    | (_,_) -> raise TypeError)
  | Mul(e1,e2) -> (match (type_of e1, type_of e2) with 
    | (IntT, IntT) -> IntT
    | (FloatT, FloatT) -> FloatT
    | (_,_) -> raise TypeError)
  | Sub(e1,e2) -> (match (type_of e1, type_of e2) with 
    | (IntT, IntT) -> IntT
    | (FloatT, FloatT) -> FloatT
    | (_,_) -> raise TypeError)
  | Div(e1,e2) -> (match (type_of e1, type_of e2) with 
    | (IntT, IntT) -> IntT
    | (FloatT, FloatT) -> FloatT
    | (_,_) -> raise TypeError)
  | Power(e1,e2) -> (match (type_of e1, type_of e2) with 
    | (IntT, IntT) -> FloatT
    | (FloatT, FloatT) -> FloatT
    | (_,_) -> raise TypeError)
  | Abs e1 -> 
    if type_of e1 = IntT then IntT
    else if type_of e1 = FloatT then FloatT
    else raise TypeError
  | Sqrt e1 -> 
    if type_of e1 = IntT then FloatT
    else if type_of e1 = FloatT then FloatT
    else raise TypeError
  | Is_equal (e1,e2) -> 
    if type_of e1 = IntT then 
      if type_of e2 = IntT then BoolT
      else raise TypeError
    else if type_of e1 = FloatT then 
      if type_of e2 = FloatT then BoolT
      else raise TypeError
    else raise TypeError
  | Is_greater (e1,e2) -> 
    if type_of e1 = IntT then 
      if type_of e2 = IntT then BoolT
      else raise TypeError
    else if type_of e1 = FloatT then 
      if type_of e2 = FloatT then BoolT
      else raise TypeError
    else raise TypeError
  | Is_lesser (e1,e2) -> 
    if type_of e1 = IntT then 
      if type_of e2 = IntT then BoolT
      else raise TypeError
    else if type_of e1 = FloatT then 
      if type_of e2 = FloatT then BoolT
      else raise TypeError
    else raise TypeError
  | Is_less_than_equal (e1,e2) -> 
    if type_of e1 = IntT then 
      if type_of e2 = IntT then BoolT
      else raise TypeError
    else if type_of e1 = FloatT then 
      if type_of e2 = FloatT then BoolT
      else raise TypeError
    else raise TypeError
  | Is_greater_than_equal (e1,e2) -> 
    if type_of e1 = IntT then 
      if type_of e2 = IntT then BoolT
      else raise TypeError
    else if type_of e1 = FloatT then 
      if type_of e2 = FloatT then BoolT
      else raise TypeError
    else raise TypeError
  | Rem (e1,e2) -> 
    if type_of e1 = IntT then 
      if type_of e2 = IntT then IntT
      else raise TypeError
    else if type_of e1 = FloatT then 
      if type_of e2 = FloatT then FloatT
      else raise TypeError
    else raise TypeError
  | Not_equal_to(e1,e2) -> 
    if type_of e1 = IntT then 
      if type_of e2 = IntT then BoolT
      else raise TypeError
    else if type_of e1 = FloatT then 
      if type_of e2 = FloatT then BoolT
      else raise TypeError
    else raise TypeError
  (* check here *)
  (* vectors and matrices *)
  | Scal_prod (e1,e2) -> (match (type_of e1, type_of e2) with 
    | (IntT, VectorT n1) -> VectorT n1
    | (FloatT, VectorT n1) -> VectorT n1
    | (IntT, MatrixT(n1,n2)) -> MatrixT(n1,n2)
    | (FloatT, MatrixT(n1,n2)) -> MatrixT(n1,n2)
    | (_,_) -> raise TypeError)
  | Dot_prod(e1,e2) -> (match (type_of e1, type_of e2) with 
    | (VectorT n1, VectorT n2) ->
      if n1 = n2 then FloatT
      else raise TypeError
    | (_,_) -> raise TypeError)
  | Angle(e1,e2) -> (match (type_of e1, type_of e2) with 
    | (VectorT n1, VectorT n2) -> 
      if n1 = n2 then FloatT
      else raise TypeError
    | (_,_) -> raise TypeError)
  | Mag e1 -> (match type_of e1 with
    | VectorT _ -> FloatT
    | _ -> raise TypeError)
  | Dim e1 -> (match type_of e1 with
    | VectorT _ -> IntT
    | _ -> raise TypeError)
  | Matrix_add(e1,e2) -> (match (type_of e1, type_of e2) with 
    | (MatrixT(n1,n2), MatrixT(m1,m2)) -> 
      if n1= m1 && n2 = m2 then MatrixT(n1,n2)
      else raise TypeError
    | (_,_) -> raise TypeError)
  | Matrix_mul(e1,e2) -> (match (type_of e1, type_of e2) with 
    | (MatrixT(n1,n2), MatrixT(m1,m2)) -> 
      if n2 = m1 then MatrixT(n1,m2)
      else raise TypeError
    | (MatrixT(d1,d2), VectorT d3) -> 
      if d2 = d3 then VectorT d1
      else raise TypeError
    | (_,_) -> raise TypeError)
  | Matrix_transpose e1 -> (match type_of e1 with
    | MatrixT(d1,d2) -> MatrixT(d2,d1)
    | _ -> raise TypeError)
  | Matrix_determinant e1 -> (match type_of e1 with
    | MatrixT(d1,d2) -> if d1 = d2 then FloatT
                        else raise TypeError
    | _ -> raise TypeError)
  | Matrix_minor(e1,i,j) -> (match type_of e1,type_of i, type_of j with
    | MatrixT(d1,d2),IntT, IntT -> 
      if d1 = 1 || d2 = 1 then raise TypeError
      else MatrixT(d1-1,d2-1)
    | _ -> raise TypeError)
  | Matrix_inverse e1 -> (match type_of e1 with
    | MatrixT(d1,d2) -> 
      if d1 = d2 then MatrixT(d1,d1)
      else raise TypeError
    | _ -> raise TypeError)
  | Input -> InputE
  | InputFile s -> InputF
  | Create_empty_matrix(d1,d2) -> MatrixT(d1,d2)
  | Vector_access(s,i) -> 
      let t = check_var s in
      (match t with
      | VectorT n -> 
        (* check out of bounds here in later stages *)
        if type_of i = IntT then FloatT
        else raise TypeError
      | _ -> raise TypeError)
  | Matrix_access(s,i,j) -> 
    let t = check_var s in
    (match t with
    | MatrixT(d1,d2) -> 
      if type_of i = IntT && type_of j = IntT then FloatT
      else raise TypeError
    | _ -> raise TypeError)
  | Matrix_row(e1) -> 
    (match type_of e1 with
      | MatrixT(_,_) -> IntT
      | _ -> raise TypeError)
  | Matrix_col(e1) ->
    (match type_of e1 with
      | MatrixT(_,_) -> IntT
      | _ -> raise TypeError)

