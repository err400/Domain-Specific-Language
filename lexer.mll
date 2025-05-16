{
    open Types
    open Parser
}

(* also support negative numbers *)
rule lexer = parse
    [' ' '\t' '\n'] {lexer lexbuf}
    | ";;" {EOF}
    | "Input()" {INPUT_E}
    | "Input(" (['a'-'z' 'A'-'Z' '0'-'9' '?' '"' '_' '.' '?' '<' '>' '*']+ as file_name) ")" {INPUT_F file_name}
    | "Print" {PRINT}
    | "true" {B true}
    | "True" {B true}
    | "TRUE" {B true}
    | "false" {B false}
    | "False" {B false}
    | "FALSE" {B false}
    (* float in scientific notation *)
    | "+" {PLUS}
    | "-" {MINUS}
    | "*" {MUL}
    | "/" {DIV}
    | '(' (['-' '+']? ['0'-'9']+ as num) ')' {INT (int_of_string num)}
    | '(' (['-' '+']? ['0'-'9'] "e" ['+' '-']? ['0'-'9']+ as float_num) ')' {FLOAT (float_of_string float_num)}
    | '(' (['-' '+']? ['0'-'9'] '.' ['0'-'9']* "E" ['+' '-']? ['0'-'9']+ as float_num) ')' {FLOAT (float_of_string float_num)}
    | '(' (['-' '+']? ['0'-'9'] '.' ['0'-'9']* "e" ['+' '-']? ['0'-'9']+ as float_num) ')' {FLOAT (float_of_string float_num)}
    | '(' (['-' '+']? ['0'-'9']+ '.' ['0'-'9']* as float_num) ')' { FLOAT (float_of_string float_num)}

    | (['0'-'9']+ as num) {INT (int_of_string num)}
    | (['0'-'9'] "e" ['+' '-']? ['0'-'9']+) as float_num {FLOAT (float_of_string float_num)}
    | (['0'-'9'] '.' ['0'-'9']* "E" ['+' '-']? ['0'-'9']+) as float_num {FLOAT (float_of_string float_num)}
    | (['0'-'9'] '.' ['0'-'9']* "e" ['+' '-']? ['0'-'9']+) as float_num {FLOAT (float_of_string float_num)}
    | (['0'-'9']+ '.' ['0'-'9']* as float_num) { FLOAT (float_of_string float_num)}

    | "[" (['0'-'9']+ ("," ['0'-'9']+)* as i_list) "]" {
        VECTOR (List.map (fun x -> Int (int_of_string x)) (String.split_on_char ',' i_list))
    }
    | "[" (['0'-'9']+ '.' ['0'-'9']* ("," ['0'-'9']+ '.' ['0'-'9']*)+ as f_list) "]" {
        VECTOR (List.map (fun x -> Float (float_of_string x)) (String.split_on_char ',' f_list))
    }
    | ":=" {ASSIGNMENT}
    | ";" {SEMI_COLON}
    | "(" {OPEN_ROUNDB}
    | ")" {CLOSE_ROUNDB}
    | "&&" {AND}
    | "||" {OR}
    | "!" {NOT}
    | "AND" {AND}
    | "and" {AND}
    | "or" {OR}
    | "OR" {OR}
    | "not" {NOT}
    | "NOT" {NOT}
    | "ABS" {ABS}
    | "SQRT" {SQRT}
    | "^" {POWER}
    | "==" {EQUAL}
    | "<" {LESS}
    | ">" {GREATER}
    | "<=" {LESS_THAN_EQUAL}
    | ">=" {GREATER_THAN_EQUAL}
    (* rem *)
    | "%" {REM}
    | "!=" {NOT_EQUAL_TO}
    | "[" {OPEN_SQB}
    | "]" {CLOSE_SQB}
    | "," {COMMA}
    | "ADD" {ADD}
    | "SCALAR_PROD" {SCALAR_PROD}
    | "DOT_PROD" {DOT_PROD}
    | "ANGLE" {ANGLE}
    | "MAG" {MAG}
    | "DIM" {DIM}
    | "bool" as s {TYPE_NAME s}
    | "int" as s {TYPE_NAME s}
    | "float" as s {TYPE_NAME s}
    | "vector" [' ']* (['0'-'9']+ as dim) {VECTOR_TYPE (int_of_string dim)}
    | "matrix" [' ']* (['0'-'9']+ as dim1) "," (['0'-'9']+ as dim2) {MATRIX_TYPE(int_of_string dim1,int_of_string dim2)}
    | "create_empty_matrix" {CREATE_EMPTY_MATRIX}
    | "matrix_add" {MATRIX_ADD}
    | "matrix_mul" {MATRIX_MUL}
    | "matrix_inverse" {MATRIX_INVERSE}
    | "matrix_minor" {MATRIX_MINOR}
    (* | "matrix_mag" {MATRIX_MAG} *)
    | "matrix_transpose" {TRANSPOSE}
    | "matrix_determinant" {DETERMINANT}
    | "vector_access" {VECTOR_ACCESS}
    | "matrix_access" {MATRIX_ACCESS}
    | "matrix_row" {MATRIX_ROW}
    | "matrix_col" {MATRIX_COL}
    | "{" {OPEN_CB}
    | "}" {CLOSE_CB}
    | "if" {IF}
    | "IF" {IF}
    | "then" {THEN}
    | "THEN" {THEN}
    | "else" {ELSE}
    | "ELSE" {ELSE}
    | "for" {FOR}
    | "FOR" {FOR}
    | "while" {WHILE}
    | "(" {OPEN_ROUNDB}
    | ")" {CLOSE_ROUNDB}
    | "(*" { multi_comment lexbuf }
    | (['a'-'z' 'A'-'Z' '0'-'9' '_' ''' '"' '.']+ as str) {V str}
    | _ {failwith ("Error String: " ^ Lexing.lexeme lexbuf)}

(* this supports multiline comments also *)
and multi_comment = parse
    | "*)" { lexer lexbuf }
    | _ { multi_comment lexbuf }

{
  let token = lexer
}
