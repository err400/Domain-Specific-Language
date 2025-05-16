(* This testcase covers all the bool operations in detail *)

bool a;
bool b;
a := Input();
b := Input();

bool not_a;
bool not_b;
not_a := not a;
not_b := not b;
Print(not_a);
Print(not_b);

bool and_a_b;
and_a_b := a && b;
Print(and_a_b);

bool or_a_b;
or_a_b := a || b;
Print(or_a_b);

;;