(* This testcase tests all the matrix operations comprehensively also handling type promotions from int to float *)

matrix 3,3 m1_int;
matrix 3,3 m2_float;
matrix 3,3 m3_mixed;
matrix 3,3 m4_mixed;


(* matrix declarations *)
m1_int := [[1,2,3],[4,5,6],[7,8,9]];
m2_float := [[1.2,3.4,5.6],[7.8,8.9,2.1],[4.3,5.4,6.5]];
m3_mixed := [[1,2,3],[7.8,8.9,2.1],[7,8,9]];
m4_mixed := [[1,2,3],[4,5,6],[7.8,8.9,2.1]];

(* scalar product of matrices *)
matrix 3,3 sm1;
matrix 3,3 sm2;
matrix 3,3 sm3;
matrix 3,3 sm4;
matrix 3,3 sm5;
matrix 3,3 sm6;

sm1 := 3 SCALAR_PROD m1_int;
sm2 := 1.5 SCALAR_PROD m1_int;
sm3 := 3 SCALAR_PROD m2_float;
sm4 := 1.5 SCALAR_PROD m2_float;
sm5 := 3 SCALAR_PROD m3_mixed;
sm6 := 1.5 SCALAR_PROD m3_mixed;

Print(sm1);
Print(sm2);
Print(sm3);
Print(sm4);
Print(sm5);
Print(sm6);

(* addition of two matrices *)
matrix 3,3 am1;
matrix 3,3 am2;
matrix 3,3 am3;
matrix 3,3 am4;
matrix 3,3 am5;
matrix 3,3 am6;

am1 := m1_int matrix_add m2_float;
am2 := m1_int matrix_add m1_int;
am3 := m2_float matrix_add m2_float;
am4 := m3_mixed matrix_add m2_float;
am5 := m3_mixed matrix_add m4_mixed;
am6 := m2_float matrix_add m4_mixed;

Print(am1);
Print(am2);
Print(am3);
Print(am4);
Print(am5);
Print(am6);

(* matrix multiplication *)

matrix 3,3 mm1;
matrix 3,3 mm2;
matrix 3,3 mm3;

matrix 4,3 mm_t1;
matrix 3,5 mm_t2;
matrix 4,5 mm_res;


mm1 := m1_int matrix_mul m1_int;
Print(mm1);

mm2 := m2_float matrix_mul m2_float;
Print(mm2);

mm3 := m1_int matrix_mul m2_float;
Print(mm3);

mm_t1 := [[1,2,3],[1,2,3],[1,2,3],[1,2,3]];
mm_t2 := [[1,2,3,1,1],[1,2,3,1,1],[1,2,3,1,1]];
mm_res := mm_t1 matrix_mul mm_t2;

Print(mm_res);


(* Transpose of a matrix *)
matrix 3,3 tm1;
matrix 3,3 tm2;
matrix 3,3 tm3;
matrix 3,3 tm4;
matrix 3,4 tm_t1;
matrix 5,3 tm_t2;
matrix 5,4 tm_res;

tm1 := matrix_transpose m1_int;
Print(tm1);

tm2 := matrix_transpose m2_float;
Print(tm2);

tm3 := matrix_transpose m3_mixed;
Print(tm3);

tm4 := matrix_transpose m4_mixed;
Print(tm4);

tm_t1 := matrix_transpose mm_t1;
Print(tm_t1);

tm_t2 := matrix_transpose mm_t2;
Print(tm_t2);

tm_res := matrix_transpose mm_res;
Print(tm_res);

(* determinant of a square matrix *)
float d1;
float d2;
float d3;
float d4;

d1 := matrix_determinant m1_int;
Print(d1);

d2 := matrix_determinant m2_float;
Print(d2);

d3 := matrix_determinant m3_mixed;
Print(d3);

d4 := matrix_determinant m4_mixed;
Print(d4);

(* Minor of a matrix *)
matrix 2,2 mat_minor1;
matrix 2,2 mat_minor2;
matrix 2,2 mat_minor3;
matrix 2,2 mat_minor4;
matrix 2,2 mat_minor5;
matrix 2,2 mat_minor6;
matrix 2,2 mat_minor7;
matrix 2,2 mat_minor8;
matrix 3,4 mat_minor9;

mat_minor1 := matrix_minor m1_int 0 0;
Print(mat_minor1);

mat_minor2 := matrix_minor m1_int 1 1;
Print(mat_minor2);

mat_minor3 := matrix_minor m1_int 2 1;
Print(mat_minor3);

mat_minor4 := matrix_minor m1_int 2 2;
Print(mat_minor4);

mat_minor5 := matrix_minor m2_float 0 0;
Print(mat_minor5);

mat_minor6 := matrix_minor m2_float 1 1;
Print(mat_minor6);

mat_minor7 := matrix_minor m3_mixed 2 1;
Print(mat_minor7);

mat_minor8 := matrix_minor m4_mixed 2 2;
Print(mat_minor8);

mat_minor9 := matrix_minor mm_res 2 2;
Print(mat_minor9);

(* Inverse of a matrix *)
matrix 3,3 mat_inv2;
matrix 3,3 mat_inv3;
matrix 3,3 mat_inv4;

mat_inv2 := matrix_inverse m2_float;
Print(mat_inv2);

mat_inv3 := matrix_inverse m3_mixed;
Print(mat_inv3);

mat_inv4 := matrix_inverse m4_mixed;
Print(mat_inv4);



int r1;
r1 := matrix_row m1_int;
Print(r1);

int r2;
r2 := matrix_row mm_res;
Print(r2);

int r3;
r3 := matrix_row tm_t2;
Print(r3);

int c1;
c1 := matrix_col m1_int;
Print(c1);

int c2;
c2 := matrix_col mm_res;
Print(c2);

int c3;
c3 := matrix_col tm_t2;
Print(c3);
;;