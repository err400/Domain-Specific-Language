(* This testcase covers all the float operations in detail *)


float x;
float y;
x := Input();
y := Input();

float add_x_y;
add_x_y := x + y;
Print(add_x_y);

float mul_x_y;
mul_x_y := x * y;
Print(mul_x_y);

float sub_x_y;
sub_x_y := x - y;
Print(sub_x_y);

float div_x_y;
div_x_y := x / y;
Print(div_x_y);

float power_x_y;
power_x_y := x ^ y;
Print(power_x_y);

float abs_x;
abs_x := ABS x;
Print(abs_x);

float abs_y;
abs_y := ABS y;
Print(abs_y);

bool is_equal_x_y;
is_equal_x_y := x == y;
bool b1;
b1 := 6 == 6;
Print(is_equal_x_y);
Print(b1);

bool is_greater_x_y;
bool is_lesser_x_y;
bool is_less_than_equal_x_y;
bool is_greater_than_equal_x_y;
bool not_equal_to_x_y;
float rem_x_y;

is_greater_x_y := x > y;
is_lesser_x_y := x < y;
is_less_than_equal_x_y := x <= y;
is_greater_than_equal_x_y := x >= y;
not_equal_to_x_y := x != y;
rem_x_y := x % y;

Print(is_greater_x_y);
Print(is_lesser_x_y);
Print(is_less_than_equal_x_y);
Print(is_greater_than_equal_x_y);
Print(not_equal_to_x_y);
Print(rem_x_y);
;;