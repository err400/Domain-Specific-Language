%{
    open Types
    open Ast
%}

%type <Ast.control> control
%type <Ast.control list> control_list
%type <Ast.exp> exp
%type <Ast.exp> factor
%type <Ast.exp> final_vector_unit
%type <Ast.exp> matrix_unit
%type <Ast.exp> combined_unit
%type <Ast.exp> first_bool_unit
%type <Ast.exp> second_bool_unit
%type <Ast.exp> third_bool_unit
%type <Ast.exp> final_bool_unit
%type <Ast.exp> first_factor
%type <Ast.exp> matrix
%type <Ast.exp> matrix_accessor
%type <Ast.exp> final1_matrix_unit
%type <Types.vector_unit list list> vector_list

(* define tokens here *)
%token INPUT_E
%token <string> INPUT_F
%token PRINT
%token <string> V 
%token<bool> B
%token<int> INT 
%token<float> FLOAT 
%token <Types.vector_unit list> VECTOR
%token <string> TYPE_NAME
%token <int> VECTOR_TYPE
%token <int * int> MATRIX_TYPE
%token AND OR NOT
%token PLUS MINUS MUL DIV ABS SQRT
%token EQUAL LESS GREATER LESS_THAN_EQUAL GREATER_THAN_EQUAL REM NOT_EQUAL_TO
%token OPEN_SQB  CLOSE_SQB POWER
%token COMMA  ADD SCALAR_PROD DOT_PROD ANGLE MAG DIM
%token MATRIX_MUL TRANSPOSE DETERMINANT MATRIX_ADD
%token MATRIX_INVERSE MATRIX_MINOR DETERMINANT CREATE_EMPTY_MATRIX
%token MATRIX_ROW MATRIX_COL
// %token MATRIX_MAG
%token EOF
%token ASSIGNMENT SEMI_COLON OPEN_CB CLOSE_CB
%token VECTOR_ACCESS MATRIX_ACCESS
%token IF THEN ELSE
%token FOR WHILE
%token OPEN_ROUNDB CLOSE_ROUNDB

(* g = n,t,p,s *)
%start exp
%start start_symbol
%type <Ast.start_symbol> start_symbol
 
%%
start_symbol:
    | control_list EOF {Control $1}

control_list:
    | control { [$1] }
    | control control_list { $1 :: $2 }

(* converts into AST nodes *)

control:
    | TYPE_NAME V SEMI_COLON {
        let t1 = type_map $1 in
        if t1 = Invalid_type then raise Types.TypeError
        else ((add_var $2 t1); Declare($2,t1))
        }
    | VECTOR_TYPE V SEMI_COLON {
        if (vector_check $1) then let t1 = (VectorT $1) in ((add_var $2 t1); Declare($2,t1))
        else raise Types.TypeError
        }
    | MATRIX_TYPE V SEMI_COLON {
        let (d1,d2) = $1 in
        if (matrix_check d1 d2) then let t1 = (MatrixT(d1,d2)) in ((add_var $2 t1); Declare($2,t1))
        else raise Types.TypeError
        }
    | V ASSIGNMENT exp SEMI_COLON {
        let t1 = check_var $1 in
        let t2 = type_of $3 in
        if t1 = t2 then Assign($1,$3)
        else if t2 = InputE then Assign($1,$3)
        else if t2 = InputF then Assign($1,$3)
        else raise Types.TypeError
        }
    | matrix_accessor ASSIGNMENT exp SEMI_COLON{
        let t1 = type_of $1 in
        let t2 = type_of $3 in
        if t1 = t2 then Matrix_assign($1,$3)
        else if t2 = InputE then Matrix_assign($1,$3)
        else if t2 = InputF then Matrix_assign($1,$3)
        else raise Types.TypeError
    }
    | OPEN_CB control_list CLOSE_CB {Seq $2}
    | IF exp THEN OPEN_CB control_list CLOSE_CB ELSE OPEN_CB control_list CLOSE_CB { 
        if type_of $2 = BoolT then If_then_else ($2,$5,$9)
        else raise Types.TypeError
        }
    (* for(i;0;n){} i++ in control_list*)
    | FOR OPEN_ROUNDB V SEMI_COLON exp SEMI_COLON exp CLOSE_ROUNDB OPEN_CB control_list CLOSE_CB {
        if check_var $3 = IntT then
            if type_of $5 = IntT && type_of $7 = IntT then For($3,$5,$7,$10)
            else raise Types.TypeError
        else raise Types.TypeError
    }
    | WHILE OPEN_ROUNDB exp CLOSE_ROUNDB OPEN_CB control_list CLOSE_CB {
        if type_of $3 = BoolT then While($3,$6)
        else raise Types.TypeError
        }
    (* check if the variable is already declared *)
    | PRINT OPEN_ROUNDB V CLOSE_ROUNDB SEMI_COLON {
        if is_declared $3 then Print $3
        else raise Types.Variable_Not_Declared
        }
    

vector_list:
    | VECTOR {[$1]}
    | VECTOR COMMA vector_list{$1::$3}


matrix:
    | OPEN_SQB vector_list CLOSE_SQB {Matrix $2}

combined_unit:
    | INT {Int_var $1}
    | FLOAT {Float_var $1}
    | OPEN_ROUNDB exp CLOSE_ROUNDB {$2}
    (* check if variable is declared or not *)
    | V { 
        if is_declared $1 then String_var $1
        else raise Types.Variable_Not_Declared}
    | B {Bool_var $1}
    | VECTOR {Vector $1}
    | matrix {$1}
    | INPUT_E{Input}
    | INPUT_F {InputFile $1}

factor:
    | combined_unit MUL factor {
        if type_of $1 = IntT then
            if type_of $3 = IntT then Mul($1,$3)
            else raise Types.TypeError
        else if type_of $1 = FloatT then 
            if type_of $3 = FloatT then Mul($1,$3)
            else raise Types.TypeError
        else raise Types.TypeError
        }
    | combined_unit DIV factor {
        if type_of $1 = IntT then
            if type_of $3 = IntT then Div($1,$3)
            else raise Types.TypeError
        else if type_of $1 = FloatT then 
            if type_of $3 = FloatT then Div($1,$3)
            else raise Types.TypeError
        else raise Types.TypeError
        }
    | combined_unit {$1}

first_factor:
    | factor MINUS first_factor {Sub($1,$3)}
    | ABS first_factor {Abs $2}
    | SQRT first_factor {Sqrt $2}
    | factor POWER first_factor {Power($1,$3)}
    | factor {$1}

first_bool_unit:
    | NOT first_bool_unit {Not $2}
    | first_factor {$1}

second_bool_unit:
    | first_bool_unit AND second_bool_unit {And($1,$3)}
    | first_bool_unit {$1}

third_bool_unit:
    | second_bool_unit {$1}
    | second_bool_unit OR third_bool_unit {Or($1,$3)}

final_bool_unit:
    | third_bool_unit{$1}
    | third_bool_unit EQUAL final_bool_unit { Is_equal($1,$3)}
    | third_bool_unit GREATER final_bool_unit{Is_greater($1,$3)}
    | third_bool_unit LESS final_bool_unit {Is_lesser($1,$3)}
    | third_bool_unit LESS_THAN_EQUAL final_bool_unit {Is_less_than_equal($1,$3)}
    | third_bool_unit GREATER_THAN_EQUAL final_bool_unit {Is_greater_than_equal($1,$3)}
    | third_bool_unit REM final_bool_unit {Rem($1,$3)}
    | third_bool_unit NOT_EQUAL_TO final_bool_unit {Not_equal_to($1,$3)}


final_vector_unit:
    | final_bool_unit{$1}
    | final_bool_unit DOT_PROD final_vector_unit {Dot_prod ($1,$3)}
    | final_bool_unit ANGLE final_vector_unit {Angle ($1,$3)}
    | MAG final_vector_unit {Mag $2}
    | DIM final_vector_unit {Dim $2}

matrix_unit:
    | final_vector_unit SCALAR_PROD matrix_unit {Scal_prod($1,$3)}
    | final_vector_unit MATRIX_MUL matrix_unit {Matrix_mul($1,$3)}
    | final_vector_unit MATRIX_ADD matrix_unit{Matrix_add($1,$3)}
    | TRANSPOSE matrix_unit {Matrix_transpose $2}
    | DETERMINANT matrix_unit {Matrix_determinant $2}
    | MATRIX_MINOR matrix_unit exp exp {Matrix_minor($2,$3,$4)}
    | MATRIX_INVERSE matrix_unit {Matrix_inverse $2}
    | MATRIX_ROW matrix_unit{Matrix_row $2}
    | MATRIX_COL matrix_unit{Matrix_col $2}
    // | MATRIX_MAG matrix_unit {Matrix_mag $2}
    | final_vector_unit{$1}

final1_matrix_unit:
    | final_vector_unit PLUS final1_matrix_unit{Add($1,$3)}
    | matrix_unit{$1}

matrix_accessor:
    | V MATRIX_ACCESS final1_matrix_unit final1_matrix_unit{Matrix_access($1,$3,$4)}
    | V VECTOR_ACCESS final1_matrix_unit {Vector_access($1,$3)}

exp:
    | final1_matrix_unit{$1}
    | CREATE_EMPTY_MATRIX INT INT{Create_empty_matrix($2,$3)}
    | matrix_accessor{$1}
    