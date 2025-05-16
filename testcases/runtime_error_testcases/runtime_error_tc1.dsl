(* runtime error : when we try to find inverse of a singular matrix *)

(* raises runtime error : Matrix is singular *)

matrix 3,3 m;
m := [[1,2,3],[4,5,6],[7,8,9]];
matrix 3,3 mat_inv;
mat_inv := matrix_inverse m;
Print(mat_inv);
;;