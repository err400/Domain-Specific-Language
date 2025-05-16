matrix 4,4 m;
m := [[1,2,3,4], [5,6,7,8], [9,10,12,13], [14,15,16,20]];

vector 4 v;
v := [1,2,3,4];

vector 4 v1;
v1 := m matrix_mul v;
Print(v1);

;;