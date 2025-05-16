(* contains the helper matrix/vertices functions required in eval function *)
open Types
open Ast

exception Division_by_zero_error;; 

let float_of_vector_unit v =
  match v with
  | Int x -> float_of_int x
  | Float x -> x

let rec foldl f e s = match s with 
  [ ] -> e
| h::t -> foldl f (f e h) t ;;

let mag_int (v : vector_unit list) =
  sqrt (foldl (fun e x ->
    match x with
    | Int i -> e +. ((float_of_int i) *. (float_of_int i))
    | _ -> raise (Failure "Expected Int")
  ) 0.0 v)

let mag_float (v : vector_unit list) =
  sqrt (foldl (fun e x ->
    match x with
    | Float f -> e +. (f *. f)
    | _ -> raise (Failure "Expected Float")
  ) 0.0 v)


(* ///////////// do type promotions here ////////// *)
(* let rec add_vector v1 v2 =  *)
(* vector addition *)
let rec add_vector_int_int v1 v2 = match (v1,v2) with
  | ([],[]) -> []
  | (Int h)::t,(Int x)::xs -> (Int (h+x))::(add_vector_int_int t xs)
  | _,_ -> raise TypeError

let rec add_vector_float_int v1 v2 = match (v1,v2) with
  | ([],[]) -> []
  | (Float h)::t,(Int x)::xs -> (Float (h +. (float_of_int x)))::(add_vector_float_int t xs)
  | _,_ -> raise TypeError

let rec add_vector_int_float v1 v2 = match (v1,v2) with
  | ([],[]) -> []
  | (Int h)::t,(Float x)::xs -> (Float ((float_of_int h) +. x))::(add_vector_int_float t xs)
  | _,_ -> raise TypeError

let rec add_vector_float_float v1 v2 = match (v1,v2) with
  | ([],[]) -> []
  | (Float h)::t,(Float x)::xs -> (Float (h +. x))::(add_vector_float_float t xs)
  | _,_ -> raise TypeError


let rec scalar_prod_vector_int_int n v = match v with
  | [] -> []
  | (Int h)::t -> (Int (h * n))::(scalar_prod_vector_int_int n t)
  | _ -> raise TypeError

let rec scalar_prod_vector_float_int n v = match v with
  | [] -> []
  | (Int h)::t -> (Float ((float_of_int h) *. n))::(scalar_prod_vector_float_int n t)
  | _ -> raise TypeError

let rec scalar_prod_vector_int_float n v = match v with
  | [] -> []
  | (Float h)::t -> (Float (h *. (float_of_int n)))::(scalar_prod_vector_int_float n t)
  | _ -> raise TypeError

let rec scalar_prod_vector_float_float n v = match v with
  | [] -> []
  | (Float h)::t -> (Float (h *. n))::(scalar_prod_vector_float_float n t)
  | _ -> raise TypeError




let rec scalar_prod_vector_int n v = match (type_of_vector v) with
  | IntT -> (scalar_prod_vector_int_int n v)
  | FloatT -> (scalar_prod_vector_int_float n v)
  | _ -> raise TypeError

let rec scalar_prod_vector_float n v = match (type_of_vector v) with
  | IntT -> (scalar_prod_vector_float_int n v)
  | FloatT -> (scalar_prod_vector_float_float n v)
  | _ -> raise TypeError

let rec scalar_prod_matrix_int n m =
  match m with
  | [] -> []
  | h :: t -> (scalar_prod_vector_int n h) :: (scalar_prod_matrix_int n t)

let rec scalar_prod_matrix_float n m =
  match m with
  | [] -> []
  | h :: t -> (scalar_prod_vector_float n h) :: (scalar_prod_matrix_float n t)


let rec vector_dot_prod_int_int v1 v2 = match (v1,v2) with
  | ([],[]) -> 0.
  | (Int h)::t,(Int x)::xs -> ((float_of_int h) *. (float_of_int x)) +. (vector_dot_prod_int_int t xs)
  | _,_ -> raise TypeError

let rec vector_dot_prod_float_int v1 v2 = match (v1,v2) with
  | ([],[]) -> 0.
  | (Float h)::t,(Int x)::xs -> (h *. (float_of_int x)) +. (vector_dot_prod_float_int t xs)
  | _,_ -> raise TypeError
  
let rec vector_dot_prod_int_float v1 v2 = match (v1,v2) with
  | ([],[]) -> 0.
  | (Int h)::t,(Float x)::xs -> ((float_of_int h) *. x) +. (vector_dot_prod_int_float t xs)
  | _,_ -> raise TypeError


let rec vector_dot_prod_float_float v1 v2 = match (v1,v2) with
  | ([],[]) -> 0.
  | (Float h)::t,(Float x)::xs -> (h *. x) +. (vector_dot_prod_float_float t xs)
  | _,_ -> raise TypeError


(*(*(* debug debug *)*)*)

let rec vector_angle_int_int v1 v2 = 
  let dot_p = vector_dot_prod_int_int v1 v2 in
  let l1 = mag_int v1 in
  let l2 = mag_int v2 in
  if (abs_float l1 < 1e-6) || (abs_float l2 < 1e-6) then raise Division_by_zero_error
  else acos (dot_p /. (l1 *. l2));;


let rec vector_angle_float_int v1 v2 = 
  let dot_p = vector_dot_prod_float_int v1 v2 in
  let l1 = mag_float v1 in
  let l2 = mag_int v2 in
  if (abs_float l1 < 1e-6) || (abs_float l2 < 1e-6) then raise Division_by_zero_error
  else acos (dot_p /. (l1 *. l2));;


let rec vector_angle_int_float v1 v2 = 
  let dot_p = vector_dot_prod_int_float v1 v2 in
  let l1 = mag_int v1 in
  let l2 = mag_float v2 in
  if (abs_float l1 < 1e-6) || (abs_float l2 < 1e-6) then raise Division_by_zero_error
  else acos (dot_p /. (l1 *. l2));;

let rec vector_angle_float_float v1 v2 = 
  let dot_p = vector_dot_prod_float_float v1 v2 in
  let l1 = mag_float v1 in
  let l2 = mag_float v2 in
  if (abs_float l1 < 1e-6) || (abs_float l2 < 1e-6) then raise Division_by_zero_error
  else acos (dot_p /. (l1 *. l2));;

let rec matrix_transpose = function
  | [] -> []
  | [] :: _ -> []
  | m -> 
      let heads = List.map List.hd m in
      let tails = List.map List.tl m in
      heads :: matrix_transpose tails
  

let matrix_minor m r c =
  let without_row = List.mapi (fun i row -> (i, row)) m  (* pairs each row with its index *)
                    |> List.filter (fun (i, _) -> i <> r)  (* removes the row with index r *)
                    |> List.map snd in     (* keep only the rows that are not r *)
  List.map (fun row ->   (* similarly this removes the column c *)
    List.mapi (fun j v -> (j, v)) row
    |> List.filter (fun (j, _) -> j <> c)
    |> List.map snd
  ) without_row

let rec matrix_determinant m =
  match m with
  | [] -> raise (Failure "Empty matrix has no determinant")
  | [ [x] ] -> float_of_vector_unit x  (* 1x1 matrix *)
  | _ ->
      let first_row = List.hd m in
      List.mapi (fun col_idx elem ->
        let sign = if col_idx mod 2 = 0 then 1.0 else -.1.0 in
        let minor = matrix_minor m 0 col_idx in
        let cofactor = sign *. float_of_vector_unit elem *. matrix_determinant minor in
        cofactor
      ) first_row
      |> List.fold_left ( +. ) 0.0
  

let matrix_cofactor m =
  List.mapi (fun i row ->
    List.mapi (fun j _ ->
      let sign = if (i + j) mod 2 = 0 then 1.0 else -.1.0 in
      let minor = matrix_minor m i j in
      Float (sign *. matrix_determinant minor)
    ) row
  ) m

let matrix_adjugate m =
  matrix_transpose (matrix_cofactor m)

let matrix_inverse m =
  let det = matrix_determinant m in
  if abs_float det < 1e-6 then raise (Failure "Matrix is singular since determinant is 0") else
  let adj = matrix_adjugate m in
  scalar_prod_matrix_float (1.0 /. det) adj


let create_empty_matrix_fn n m =
  let row = List.init m (fun _ -> (Float 0.0)) in
  List.init n (fun _ -> row)


(*
matrix_inverse
*)