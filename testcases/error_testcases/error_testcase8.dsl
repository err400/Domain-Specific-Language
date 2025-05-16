(* ill typed program *)

matrix 3,3 m;
m := [[1,2,3],[4,5,6],[7,8,9]];
m := [3,4] SCALAR_PROD m;
Print(m);
;;