matrix 4,4 m;
m := Input();

float det_of_m;
det_of_m := matrix_determinant m;

vector 4 b;
b := Input();

if (det_of_m != 0.) then {
    matrix 4,4 inv;
    inv := matrix_inverse m;
    Print(inv);
    vector 4 x;
    x := inv matrix_mul b;
    Print(x);
}
else{
    Print(det_of_m);
}
;;