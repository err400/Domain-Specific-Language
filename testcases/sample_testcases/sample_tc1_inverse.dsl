matrix 4,4 m;
m := Input();
float det_of_m;
det_of_m := matrix_determinant m;
if (det_of_m != 0.) then {
    Print(det_of_m);
    matrix 4,4 cofactor_matrix;
    cofactor_matrix := create_empty_matrix 4 4;
    int i;
    int j;
    for(i;0;3){
        for(j;0;3){
            matrix 3,3 m1;
            m1 := matrix_minor m i j;
            float d1;
            d1 := matrix_determinant m1;
            cofactor_matrix matrix_access i j := ((-1)^(i+j))*d1;
        }
    }
    Print(cofactor_matrix);
    matrix 4,4 adj_matrix;
    matrix 4,4 inv_matrix;
    adj_matrix := matrix_transpose cofactor_matrix;
    float dd;
    dd := 1. / det_of_m;
    inv_matrix := dd SCALAR_PROD adj_matrix;
    Print(inv_matrix);
}
else{
    int x;
    x:= 4;
}
;;