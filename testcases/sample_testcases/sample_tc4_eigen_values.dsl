(* Eigenvalues of 2x2 matrix [ [a, b], [c, d] ] *)
matrix 2,2 m;
m := [[1.2,3.4],[5.6,7.8]];
float trace;
trace := (m matrix_access 0 0) + (m matrix_access 1 1);
float det;
det := matrix_determinant m;
float test;
test := (trace*trace) - (4. * det);

if (test >= 0.) then {
    float eigen_value1;
    float eigen_value2;
    float s;
    s := SQRT test;
    eigen_value1 := (trace + s) / 2.;
    eigen_value2 := (trace - s) / 2.;
    Print(eigen_value1);
    Print(eigen_value2);
}
else{
    bool f;
    f := false;
    Print(f);
}

;;