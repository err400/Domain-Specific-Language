(* variables table - stores the value of variables *)
open Types
open Ast
open Functions

type values = B of bool | N of int | F of float | V of vector_unit list | M of vector_unit list list;;

let variable_table : (string,values) Hashtbl.t = Hashtbl.create 10

let rho s = 
  try Hashtbl.find variable_table s 
  with Not_found -> raise Variable_has_no_value;;

let rec add_vectors v1 v2 = match (type_of_vector v1, type_of_vector v2) with
  | IntT, IntT -> (add_vector_int_int v1 v2)
  | FloatT, IntT -> (add_vector_float_int v1 v2)
  | IntT, FloatT -> (add_vector_int_float v1 v2)
  | FloatT, FloatT -> (add_vector_float_float v1 v2)
  | _, _ -> raise TypeError

let rec matrix_add m1 m2 =
  match (m1, m2) with
  | ([], []) -> []
  | (r1 :: t1, r2 :: t2) -> (add_vectors r1 r2) :: matrix_add t1 t2
  | _,_ -> raise TypeError


let rec dot_product v1 v2 = match (type_of_vector v1, type_of_vector v2) with
  | IntT, IntT -> Float (vector_dot_prod_int_int v1 v2)
  | FloatT, FloatT -> Float (vector_dot_prod_float_float v1 v2)
  | IntT, FloatT -> Float (vector_dot_prod_int_float v1 v2)
  | FloatT, IntT -> Float (vector_dot_prod_float_int v1 v2)
  | _, _ -> raise TypeError

let rec matrix_vector_mul m v = match m with
  | [] -> []
  | h :: t -> let dot = dot_product h v in dot :: (matrix_vector_mul t v)

let rec matrix_mul m1 m2 =
  let m2_t = matrix_transpose m2 in
  match m1 with
  | [] -> []
  | r1 :: t1 ->
      let rec mul_helper row cols =
        match cols with
        | [] -> []
        | col :: rest_cols ->
            let dot = dot_product row col in
            dot :: mul_helper row rest_cols
      in
      let row_result = mul_helper r1 m2_t in
      row_result :: matrix_mul t1 m2
  
let vector_access_fn vname n1 = 
  match Hashtbl.find variable_table vname with
    | V v ->
        if n1 < 0 || n1 >= dim_of_vector v then
          raise (Failure "Vector index out of bounds")
        else
          (let value = List.nth v n1 in
          match value with
            | Int i -> float_of_int i
            | Float f -> f)
    | _ -> raise (Failure "Variable is not a vector")

let matrix_access_fn vname n1 n2 = 
  match Hashtbl.find variable_table vname with
  | M m ->
      if n1 < 0 || n1 >= List.length m then
        raise (Failure "Matrix row index out of bounds")
      else
        let row = List.nth m n1 in
        if n2 < 0 || n2 >= List.length row then
          raise (Failure "Matrix column index out of bounds")
        else
          let value = List.nth row n2 in
          (match value with
            | Int i -> float_of_int i
            | Float f -> f)
  | _ -> raise (Failure "Variable is not a matrix")
    
let rec update_vector_element vec i new_elem =
  match vec with
    | [] -> raise (Failure "Index out of bounds")
    | x :: xs ->
        if i = 0 then new_elem :: xs
        else x :: update_vector_element xs (i - 1) new_elem
  

let rec update_mat_element mat i j new_elem =
  match mat with
  | [] -> raise (Failure "Row index out of bounds")
  | row :: rows ->
      if i = 0 then
        let updated_row = update_vector_element row j new_elem in
        updated_row :: rows
      else
        row :: update_mat_element rows (i - 1) j new_elem


let parse_exp_input (s : string) : Ast.exp =
  let lexbuf = Lexing.from_string s in
  Parser.exp Lexer.token lexbuf

(* | Input_stdout | Input_file_open of string *)

let rec print_vector v =
  match v with
  | [] -> ""
  | [Int i] -> string_of_int i
  | [Float f] -> Printf.sprintf "%.2f" f
  | Int i :: rest -> string_of_int i ^ ", " ^ print_vector rest
  | Float f :: rest -> Printf.sprintf "%.2f" f ^ ", " ^ print_vector rest

let print_matrix m =
  let rows = List.map (fun row -> "[" ^ print_vector row ^ "]") m in
  "[" ^ String.concat "," rows ^ "]"



let rec eval e = match e with
  (* this should return the variable mapping from the table *)
  | String_var s -> rho s 
  | Bool_var b -> B b
  | Int_var i -> N i
  | Float_var f -> F f 
  | Vector v -> V v 
  | Matrix m -> M m 
  | Not e1 -> 
    (match eval e1 with
    | B b -> B (not b)
    | _ -> raise (Failure "Expected boolean value")
    )
  | And(e1,e2) -> 
    (
      match (eval e1, eval e2) with
      | B b1, B b2 -> B (b1 && b2)
      | _,_ -> raise (Failure "Expected boolean value")
    )
  | Or(e1,e2) -> 
    (
      match (eval e1, eval e2) with
      | B b1, B b2 -> B (b1 || b2)
      | _,_ -> raise (Failure "Expected boolean value")
    )
  (* type promotion here *)
  | Add(e1,e2) ->
    (
      match(eval e1, eval e2) with 
      | N n1, N n2 -> N (n1+n2)
      | F f1, F f2 -> F (f1 +. f2)
      | V v1, V v2 -> 
        (match (type_of_vector v1, type_of_vector v2) with
          | IntT, IntT -> V (add_vector_int_int v1 v2)
          | FloatT, IntT -> V (add_vector_float_int v1 v2)
          | IntT, FloatT -> V (add_vector_int_float v1 v2)
          | FloatT, FloatT -> V (add_vector_float_float v1 v2)
          | _,_ -> raise TypeError)
      | _,_ -> raise (Failure "+ is only compatible with int,float and vector types")
    )
  | Mul(e1,e2) ->
    (
      match(eval e1, eval e2) with 
      | N n1, N n2 -> N (n1*n2)
      | F f1, F f2 -> F (f1 *. f2)
      | _,_ -> raise (Failure "* is only compatible with int and float types")
    )
  | Sub(e1,e2) ->
    (
      match(eval e1, eval e2) with 
      | N n1, N n2 -> N (n1 - n2)
      | F f1, F f2 -> F (f1 +. (-1. *. f2))
      | _,_ -> raise (Failure "- is only compatible with int and float types")
    )
  | Div(e1,e2) ->
    (
      match(eval e1, eval e2) with 
      | N n1, N n2 -> 
        if(n2 = 0) then raise (Failure "Division by Zero")
        else N (n1/n2)
      | F f1, F f2 -> 
        if(f2 = 0.) then raise (Failure "Division by Zero")
        else F (f1 /. f2)
      | _,_ -> raise (Failure "/ is only compatible with int and float types")
    )
  | Power(e1,e2) ->
    (
      match(eval e1, eval e2) with 
      | N n1, N n2 -> F ((float_of_int n1) ** (float_of_int n2))
      | F f1, F f2 -> F (f1 ** f2)
      | _,_ -> raise (Failure "Power(^) is only compatible with int and float types")
    )
  | Abs e1 ->
    (
      match eval e1 with
      | N n -> N ( abs n)
      | F f -> F (abs_float f)
      | _ -> raise TypeError (* debug - raise good error (think of it) *)
    )
  | Sqrt e1 ->
    (
      match eval e1 with
      | N n -> 
          if n < 0 then raise (Failure "Square root of negative number")
          else F (sqrt (float_of_int n))
      | F f -> 
          if f < 0.0 then raise (Failure "Square root of negative number")
          else F (sqrt f)
      | _ -> raise TypeError
    )
  | Is_equal(e1,e2) ->
    (
      match(eval e1, eval e2) with
      | N n1, N n2 -> if(n1 = n2) then B true
      else B false
      | F f1, F f2 -> if (f1 = f2) then B true
      else B false
      | _,_ -> raise TypeError
    )
  | Is_greater(e1,e2) ->
    (
      match(eval e1, eval e2) with
      | N n1, N n2 -> if(n1 > n2) then B true
      else B false
      | F f1, F f2 -> if (f1 > f2) then B true
      else B false
      | _,_ -> raise TypeError
    )
  | Is_lesser(e1,e2) ->
    (
      match(eval e1, eval e2) with
      | N n1, N n2 -> if(n1 < n2) then B true
      else B false
      | F f1, F f2 -> if (f1 < f2) then B true
      else B false
      | _,_ -> raise TypeError
    )
  | Is_less_than_equal(e1,e2) ->
    (
      match(eval e1, eval e2) with
      | N n1, N n2 -> if(n1 <= n2) then B true
      else B false
      | F f1, F f2 -> if (f1 <= f2) then B true
      else B false
      | _,_ -> raise TypeError
    )
  | Is_greater_than_equal(e1,e2) ->
    (
      match(eval e1, eval e2) with
      | N n1, N n2 -> if(n1 >= n2) then B true
      else B false
      | F f1, F f2 -> if (f1 >= f2) then B true
      else B false
      | _,_ -> raise TypeError
    )
  | Rem(e1,e2) ->
    (
      match(eval e1, eval e2) with
      | N n1, N n2 -> N (n1 mod n2)
      | F f1, F f2 -> F (mod_float f1 f2)
      | _,_ -> raise TypeError
    )
  | Not_equal_to(e1,e2) ->
    (
      match(eval e1, eval e2) with
      | N n1, N n2 -> if (n1 <> n2) then B true
      else B false
      | F f1, F f2 -> if (f1 <> f2) then B true
      else B false
      | _,_ -> raise TypeError
    )
  | Scal_prod(e1,e2) -> 
    (
      match(eval e1, eval e2) with
      | N n1, V v1 -> 
        (match type_of_vector v1 with
        | IntT -> V (scalar_prod_vector_int_int n1 v1)
        | FloatT ->  V (scalar_prod_vector_int_float n1 v1)
        | _ -> raise TypeError)
      | F f1, V v1 -> 
        (match type_of_vector v1 with
        | IntT -> V (scalar_prod_vector_float_int f1 v1)
        | FloatT ->  V (scalar_prod_vector_float_float f1 v1)
        | _ -> raise TypeError)

      | N n1, M m1 -> M (scalar_prod_matrix_int n1 m1)   (* debug1 *)
      | F f1, M m1 -> M (scalar_prod_matrix_float f1 m1)  (* debug1 *)
      | _,_ -> raise TypeError
    )
  | Dot_prod(e1,e2) ->
    (
      match(eval e1, eval e2) with
      | V v1, V v2 -> 
        (* debug1 *)
        (match (type_of_vector v1, type_of_vector v2) with 
          | IntT, IntT -> F (vector_dot_prod_int_int v1 v2)
          | FloatT, FloatT -> F (vector_dot_prod_float_float v1 v2)
          | IntT, FloatT -> F (vector_dot_prod_int_float v1 v2)
          | FloatT, IntT -> F (vector_dot_prod_float_int v1 v2)
          | _,_ -> raise TypeError)
      | _,_ -> raise TypeError
    )
  (* tp here *)
  | Angle(e1,e2) ->
    (
      match(eval e1, eval e2) with
      | V v1, V v2 -> 
        (match (type_of_vector v1, type_of_vector v2) with 
          | IntT, IntT -> F (vector_angle_int_int v1 v2)
          | FloatT, FloatT -> F (vector_angle_float_float v1 v2)
          | IntT, FloatT -> F (vector_angle_int_float v1 v2)
          | FloatT, IntT -> F (vector_angle_float_int v1 v2)
          | _,_ -> raise TypeError)
      | _,_ -> raise TypeError
    )
  (* tp here *)
  | Mag e1 ->
    (
      match eval e1 with
      | V v1 ->
        (match type_of_vector v1 with
          | IntT -> F (mag_int v1)
          | FloatT -> F (mag_float v1)
          | _ -> raise TypeError)
      | _ -> raise TypeError
    )
  | Dim e1 -> 
    (
      match eval e1 with
      | V v1 -> N (dim_of_vector v1)
      | _ -> raise TypeError
    )
  | Matrix_add(e1,e2) ->
    (
      match(eval e1, eval e2) with
      | M m1, M m2 -> M (matrix_add m1 m2) (* debug1 *)
      | _,_ -> raise TypeError
    )
  (* tp here *)
  | Matrix_mul(e1,e2) ->
    (
      match(eval e1, eval e2) with
      | M m1, M m2 -> M (matrix_mul m1 m2) (* debug1 *)
      | M m1, V v1 -> V (matrix_vector_mul m1 v1) (* debug1 *)
      | _,_ -> raise TypeError
    )
  | Matrix_transpose e1 ->
    (match eval e1 with
      | M m -> M (matrix_transpose m) (* debug1 *)
      | _ -> raise TypeError
    )
  | Matrix_determinant e1 ->
    (
      match eval e1 with
      | M m -> F (matrix_determinant m) (* debug *)
      | _ -> raise TypeError
    )
  | Matrix_minor(e1,e2,e3) ->
    (
      match (eval e1, eval e2, eval e3) with
      | M m1, N n1, N n2 -> 
        (* check bounds of i, j with the matrix dimension here *)
        (match type_of e1 with 
        | MatrixT(d1,d2) -> if (n1 >= d1) || (n1 < 0) || (n2 >= d2) || (n2 < 0) then raise TypeError (* debug *)
                            else M (matrix_minor m1 n1 n2)
        | _ -> raise TypeError)
      | _, _ , _ -> raise TypeError
    )
  | Matrix_inverse e1 -> 
    (
      match eval e1 with
      | M m1 -> M (matrix_inverse m1) (* debug *)
      | _ -> raise TypeError
    )
  (* read a line from terminal and assign the variable accordingly *)
  | Input -> 
    print_string "> ";
    flush stdout;
    let line = read_line () in
    let parsed_exp = parse_exp_input line in
    eval parsed_exp
  | InputFile filename ->
    let chan = open_in filename in
    let line = input_line chan in
    close_in chan;
    let parsed_exp = parse_exp_input line in
    eval parsed_exp
  | Create_empty_matrix(n1,n2) -> (M (create_empty_matrix_fn n1 n2)) (* debug1 *)
  | Vector_access(s,e1) -> 
    (* check out of bounds here *)
    (
      match eval e1 with
      | N n1 -> F (vector_access_fn s n1) (* debug1 *)
      | _ -> raise TypeError
    )
  | Matrix_access(s,e1,e2) -> 
    (
      match (eval e1, eval e2) with 
      | N n1, N n2 -> F (matrix_access_fn s n1 n2) (* debug1 *)
      | _,_ -> raise TypeError
    )
  | Matrix_row(e1) -> 
    (match eval e1 with
      | M m -> let (x,y) = dim_of_matrix m in N x
      | _ -> raise TypeError)
  | Matrix_col(e1) ->
    (match eval e1 with
      | M m -> let (x,y) = dim_of_matrix m in N y
      | _ -> raise TypeError)


let rec eval_control c = match c with
  (* store the variable in the hash table *)
  | Assign(s,e) -> 
    (* create mapping from variable to the evaluation of the RHS *)
    (* type check the LHS and RHS in case of Input *)
    (match e with
      | Input -> 
        (* first call the eval function *)
        let val1 = (eval e) in 
        let var_type = check_var s in
        let exp_type = 
          match val1 with
            | N _ -> IntT
            | F _ -> FloatT
            | B _ -> BoolT
            | V v -> 
              let d = dim_of_vector v in (VectorT d)
            | M m -> 
              let (x,y) = dim_of_matrix m in MatrixT (x, y)
        in 
        if var_type = exp_type then 
          Hashtbl.replace variable_table s val1
        else raise TypeError
      | _ -> let val1 = eval e in 
            Hashtbl.replace variable_table s val1)
  | Matrix_assign(e1,e2) -> 
    (match e2 with
    | Input ->
      let val1 = eval e2 
      in 
      let exp_type = 
        (match val1 with
          | N _ -> IntT
          | F _ -> FloatT
          | B _ -> BoolT
          | V v -> 
            let d = dim_of_vector v in (VectorT d)
          | M m -> 
            let (x,y) = dim_of_matrix m in MatrixT(x, y))
      in 
      (match e1 with
        | Vector_access(s,e) ->
          let var_type = check_var s in
          if var_type = exp_type then 
            let i = 
              match eval e with
              | N n -> n 
              | _ -> raise TypeError
            in
            let vect  =
              match Hashtbl.find variable_table s with
                | V v -> v 
                | _ -> raise TypeError
            in
            let new_ele =
              match val1 with
              | N n -> Int n
              | F f -> Float f
              | _ -> raise TypeError
            in
            let updated_vector = update_vector_element vect i new_ele in  (* debug - make this function *)
            Hashtbl.replace variable_table s (V updated_vector)
          else raise TypeError
        | Matrix_access(s,e3,e4) ->
          let i = 
            match eval e3 with
            | N n -> n 
            | _ -> raise TypeError
          in 
          let j = 
            match eval e4 with
            | N n -> n 
            | _ -> raise TypeError
          in
          let mat  =
            match Hashtbl.find variable_table s with
              | M m -> m
              | _ -> raise TypeError
          in
          let new_ele =
            match val1 with
            | N n -> Int n
            | F f -> Float f
            | _ -> raise TypeError
          in
          let updated_mat = update_mat_element mat i j new_ele in  (* debug - make this function *)
          Hashtbl.replace variable_table s (M updated_mat)
        | _ -> raise TypeError)
    | _ -> 
      let val1 = eval e2 in 
      (match e1 with
        | Vector_access(s,e) ->
          let i = 
            match eval e with
            | N n -> n 
            | _ -> raise TypeError
          in
          let vect  =
            match Hashtbl.find variable_table s with
              | V v -> v 
              | _ -> raise TypeError
          in
          let new_ele =
            match val1 with
            | N n -> Int n
            | F f -> Float f
            | _ -> raise TypeError
          in
          let updated_vector = update_vector_element vect i new_ele in  (* debug - make this function *)
          Hashtbl.replace variable_table s (V updated_vector)
        | Matrix_access(s,e3,e4) ->
          let i = 
            match eval e3 with
            | N n -> n 
            | _ -> raise TypeError
          in 
          let j = 
            match eval e4 with
            | N n -> n 
            | _ -> raise TypeError
          in
          let mat  =
            match Hashtbl.find variable_table s with
              | M m -> m
              | _ -> raise TypeError
          in
          let new_ele =
            match val1 with
            | N n -> Int n
            | F f -> Float f
            | _ -> raise TypeError
          in
          let updated_mat = update_mat_element mat i j new_ele in  (* debug - make this function *)
          Hashtbl.replace variable_table s (M updated_mat)
        | _ -> raise TypeError)
  )
  | Declare(s,t) -> ()
  | Seq c -> List.iter eval_control c
  | If_then_else(e1,c1,c2) -> 
    (
      match eval e1 with
      | B true -> List.iter eval_control c1
      | B false -> List.iter eval_control c2
      | _ -> raise TypeError
    )
  (* change the iterator variable in the loop here - debug - simulate a for loop here *)
  | For(s,e1,e2,c) ->
    (
      match (eval e1,eval e2) with
        | N n1, N n2 -> 
          if n1 <= n2 then 
            for i = n1 to n2 do 
              Hashtbl.replace variable_table s (N i);
              List.iter eval_control c
            done
          else
            for i = n1 downto n2 do
              Hashtbl.replace variable_table s (N i);
              List.iter eval_control c
            done
        | _,_ -> raise TypeError
    )
  | While(e1,c1) ->
    let rec while_loop() = 
      match eval e1 with
        | B true ->
          List.iter eval_control c1;
          while_loop()
        | B false -> ()
        | _ -> raise TypeError
      in 
      while_loop()
  (* print the eval of variable declared *)
  | Print(s) -> 
    let val1 = (rho s) in 
    match val1 with
    | N n -> Printf.printf "%s = %d\n" s n
    | F f -> Printf.printf "%s = %f\n" s f
    | B b -> Printf.printf "%s = %b\n" s b
    | V v -> Printf.printf "%s = [%s]\n" s (print_vector v)
    | M m -> Printf.printf "%s = %s\n" s (print_matrix m)
