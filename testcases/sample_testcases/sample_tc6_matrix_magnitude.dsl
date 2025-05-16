matrix 4,5 m;
m := [[1,2,3,4,5], [5,6,7,8,9], [9,10,12,13,11], [34,14,15,16,20]];

float sum_of_squares;
sum_of_squares := 0.;

int i;
int j;
for(i;0;3){
    for(j;0;4){
        sum_of_squares := sum_of_squares + ((m matrix_access i j) * (m matrix_access i j));
    }
}

float mag_of_matrix;
mag_of_matrix := SQRT sum_of_squares;
Print(mag_of_matrix);
;;