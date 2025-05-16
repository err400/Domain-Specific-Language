(* This testcase tests all the vector operations comprehensively also handling type promotions from int to float *)

(* checking type promotions of vectors *)
vector 3 v1_int;
v1_int := [1,2,3];

vector 3 v2_int;
v2_int := [5,6,7];

vector 3 v1_float;
v1_float := [4.6,5.6,3.4];

vector 3 v2_float;
v2_float := [1.2,3.4,5.6];

(* vector sum type promotion check *)
vector 3 sv1;
sv1 := v1_int + v2_int;

vector 3 sv2;
sv2 := v1_float + v2_int;

vector 3 sv3;
sv3 := v1_int + v2_float;

vector 3 sv4;
sv4 := v1_float + v2_float;

Print(sv1);
(* [6, 8, 10] *)

Print(sv2);
(* [9.60, 11.60, 10.40] *)

Print(sv3);
(* [2.20, 5.40, 8.60] *)

Print(sv4);
(* [5.80, 9.00, 9.00] *)


(* Scalar Product of vectors type promotion *)
vector 3 tv1;
vector 3 tv2;
vector 3 tv3;
vector 3 tv4;

tv1 := 3 SCALAR_PROD v1_int;
Print(tv1);
(* [3, 6, 9] *)

tv2 := 3 SCALAR_PROD v1_float;
Print(tv2);
(* [13.80, 16.80, 10.20] *)

tv3 := 8.4 SCALAR_PROD v1_int;
Print(tv3);
(* [8.40, 16.80, 25.20] *)

tv4 := 8.4 SCALAR_PROD v1_float;
Print(tv4);
(* [38.64, 47.04, 28.56] *)

(* type promotion in dot product b/w two vectors of different types *)
float dv1;
float dv2;
float dv3;
float dv4;

dv1 := v1_int DOT_PROD v2_int;
dv2 := v1_float DOT_PROD v2_int;
dv3 := v1_int DOT_PROD v2_float;
dv4 := v1_float DOT_PROD v2_float;

Print(dv1);
(* 38.000000 *)

Print(dv2);
(* 80.400000 *)

Print(dv3);
(* 24.800000 *)

Print(dv4);
(* 43.600000 *)

(* testing type promotion in angle of two vectors *)
float a1;
float a2;
float a3;
float a4;

a1 := v1_int ANGLE v2_int;
a2 := v1_float ANGLE v2_int;
a3 := v1_int ANGLE v2_float;
a4 := v1_float ANGLE v2_float;

Print(a1);
(* 0.252345 *)

Print(a2);
(* 0.292134 *)

Print(a3);
(* 0.098450 *)

Print(a4);
(* 0.613276 *)

(* testing magnitude function of vectors *)

float m1;
float m2;
float m3;
float m4;

m1 := MAG v1_int;
m2 := MAG v2_int;
m3 := MAG v1_float;
m4 := MAG v2_float;


Print(m1);
(* 3.741657 *)

Print(m2);
(* 10.488088 *)

Print(m3);
(* 8.004998 *)

Print(m4);
(* 6.660330 *)

int d1;
int d2;

d1 := DIM v1_int;
(* 3 *)

Print(d1);

vector 5 v1t;
v1t := [1,2,3,4,5];
d2 := DIM v1t;
Print(d2);
(* 5 *)

;;