matrix 2,2 m;
m := [[1,2],[3,4]];

float sum_of_squares;
sum_of_squares := 0.;

int i;
int j;
for(i;0;1){
    for(j;0;1){
        sum_of_squares := sum_of_squares + ((m matrix_access i j) * (m matrix_access i j));
    }
}

float mag_of_matrix;
mag_of_matrix := SQRT sum_of_squares;

float norm_diff;
norm_diff := mag_of_matrix;

float threshold;
threshold := 1.0;

while(norm_diff > threshold){
    m := m matrix_mul m;
    norm_diff := norm_diff - threshold;
}
Print(m);
;;