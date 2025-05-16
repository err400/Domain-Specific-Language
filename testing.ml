open Lexing
open Parser
open Types
open Ast
open Eval

(*(*(* *)*)*)
let rec string_of_vector v =
  match v with
  | [] -> ""
  | [Int i] -> string_of_int i
  | [Float f] -> Printf.sprintf "%.2f" f
  | Int i :: rest -> string_of_int i ^ ", " ^ string_of_vector rest
  | Float f :: rest -> Printf.sprintf "%.2f" f ^ ", " ^ string_of_vector rest

let string_of_matrix m =
  let rows = List.map (fun row -> "[" ^ string_of_vector row ^ "]") m in
  "[" ^ String.concat "," rows ^ "]"

let rec show_exp = function
  | Int_var i -> Printf.sprintf "Int_var(%d)" i
  | Float_var f -> Printf.sprintf "Float_var(%f)" f
  | Bool_var b -> Printf.sprintf "Bool_var(%b)" b
  | String_var s -> Printf.sprintf "String_var(%s)" s
  | Vector v -> Printf.sprintf "Vector[%s]" (string_of_vector v)
  | Matrix m -> Printf.sprintf "Matrix[%s]" (string_of_matrix m)
  | Not e1 -> Printf.sprintf "Not(%s)" (show_exp e1) 
  | And (e1, e2) -> Printf.sprintf "And(%s, %s)" (show_exp e1) (show_exp e2)
  | Or (e1, e2) -> Printf.sprintf "Or(%s, %s)" (show_exp e1) (show_exp e2)
  | Add (e1, e2) -> Printf.sprintf "Add(%s, %s)" (show_exp e1) (show_exp e2)
  | Mul (e1, e2) -> Printf.sprintf "Mul(%s, %s)" (show_exp e1) (show_exp e2)
  | Sub (e1, e2) -> Printf.sprintf "Sub(%s, %s)" (show_exp e1) (show_exp e2)
  | Div (e1, e2) -> Printf.sprintf "Div(%s, %s)" (show_exp e1) (show_exp e2)
  | Power (e1, e2) -> Printf.sprintf "Power(%s, %s)" (show_exp e1) (show_exp e2)
  | Abs e -> Printf.sprintf "Abs(%s)" (show_exp e)
  | Is_equal (e1, e2) -> Printf.sprintf "Is_equal(%s, %s)" (show_exp e1) (show_exp e2)
  | Is_greater (e1, e2) -> Printf.sprintf "Is_greater(%s, %s)" (show_exp e1) (show_exp e2)
  | Is_lesser (e1, e2) -> Printf.sprintf "Is_lesser(%s, %s)" (show_exp e1) (show_exp e2)
  | Is_less_than_equal (e1, e2) -> Printf.sprintf "Is_less_than_equal(%s, %s)" (show_exp e1) (show_exp e2)
  | Is_greater_than_equal (e1, e2) -> Printf.sprintf "Is_greater_than_equal(%s, %s)" (show_exp e1) (show_exp e2)
  | Rem (e1, e2) -> Printf.sprintf "Rem(%s, %s)" (show_exp e1) (show_exp e2)
  | Not_equal_to (e1, e2) -> Printf.sprintf "Not_equal_to(%s, %s)" (show_exp e1) (show_exp e2)
  | Scal_prod (e1, e2) -> Printf.sprintf "Scal_prod(%s, %s)" (show_exp e1) (show_exp e2)
  | Dot_prod (e1, e2) -> Printf.sprintf "Dot_prod(%s, %s)" (show_exp e1) (show_exp e2)
  | Angle (e1, e2) -> Printf.sprintf "Angle(%s, %s)" (show_exp e1) (show_exp e2)
  | Mag e -> Printf.sprintf "Mag(%s)" (show_exp e)
  | Dim e -> Printf.sprintf "Dim(%s)" (show_exp e)
  | Matrix_add (e1, e2) -> Printf.sprintf "Matrix_add(%s, %s)" (show_exp e1) (show_exp e2)
  | Matrix_mul (e1, e2) -> Printf.sprintf "Matrix_mul(%s, %s)" (show_exp e1) (show_exp e2)
  | Matrix_transpose e -> Printf.sprintf "Matrix_transpose(%s)" (show_exp e)
  | Matrix_determinant e -> Printf.sprintf "Matrix_determinant(%s)" (show_exp e)
  (* | Matrix_mag e -> Printf.sprintf "Matrix_mag(%s)" (show_exp e) *)
  | Matrix_minor(e,i,j) -> Printf.sprintf "Matrix_minor(%s,%s,%s)" (show_exp e) (show_exp i) (show_exp j) 
  | Matrix_inverse e -> Printf.sprintf "Matrix_inverse(%s)" (show_exp e)
  | Input -> Printf.sprintf "Input"
  | InputFile s -> Printf.sprintf "InputFile(%s)" s
  (* | Create_empty_matrix(d1,d2) -> Printf.sprintf "Create_empty_matrix(%d, %d)" d1 d2 *)
  | Vector_access(s,i) -> Printf.sprintf "Vector_access(%s, %s)" s (show_exp i)
  | Matrix_access(s,i,j) -> Printf.sprintf "Matrix_access(%s, %s, %s)" s (show_exp i) (show_exp j)

let rec type_to_string t = match t with
  | BoolT -> "BoolT" 
  | IntT -> "IntT"
  | FloatT -> "FloatT"
  | VectorT n -> Printf.sprintf "VectorT(%d)" n
  | MatrixT(d1,d2) -> Printf.sprintf "MatrixT(%d,%d)" d1 d2
  | _ -> raise TypeError

let rec show_control = function
  | Assign(v, e) -> Printf.sprintf "Assign(%s, %s)" v (show_exp e)
  (* | Matrix_assign(e1, e2) -> Printf.sprintf "Matrix_assign(%s, %s)" (show_exp e1) (show_exp e2) *)
  | Declare(s,t) -> Printf.sprintf "Declare(%s, %s)" s (type_to_string t)
  | Seq lst -> Printf.sprintf "Seq[%s]" (String.concat "; " (List.map show_control lst))
  | If_then_else(e,c1,c2) -> Printf.sprintf "If_then_else(%s, %s, %s)" (show_exp e) (String.concat "; " (List.map show_control c1)) (String.concat "; " (List.map show_control c2))
  | For(s,e1,e2,c1) -> Printf.sprintf "For(%s, %s,%s, %s)" s (show_exp e1) (show_exp e2) (String.concat "; " (List.map show_control c1))
  | While(e1,c1) -> Printf.sprintf "While(%s, %s)" (show_exp e1) (String.concat "; " (List.map show_control c1))
  | Print s -> Printf.sprintf "Print(%s)" s

let show_start_symbol (ast : start_symbol) =
  match ast with
  | Control c -> String.concat "\n" (List.map show_control c)

(* let parse_input () =
  let lexbuf = Lexing.from_channel stdin in
  try
    let ast = Parser.start_symbol Lexer.lexer lexbuf in
    Printf.printf "Parsed AST:\n%s\n" (show_start_symbol ast) 
  with
  | Parsing.Parse_error ->
      let pos = Lexing.lexeme_start_p lexbuf in
      Printf.printf "Syntax error at line %d, column %d\n"
        pos.pos_lnum (pos.pos_cnum - pos.pos_bol);
      exit 1

let () =
  Printf.printf "Enter input:\n";
  flush stdout;
  parse_input () *)

let parse_and_eval_input () =
  let lexbuf = Lexing.from_channel stdin in
  try
    let ast = Parser.start_symbol Lexer.lexer lexbuf in
    Printf.printf "Parsed AST:\n%s\n" (show_start_symbol ast);
    (* ⬇️ Evaluate the control commands now *)
    match ast with
    | Control c_list ->
        Eval.eval_control (Seq c_list)
  with
    | Parsing.Parse_error ->
        let pos = Lexing.lexeme_start_p lexbuf in
        Printf.printf "Syntax error at line %d, column %d\n"
          pos.pos_lnum (pos.pos_cnum - pos.pos_bol);
        exit 1
    | Types.TypeError ->
        Printf.printf "Type error during evaluation\n";
        exit 1
    | Types.Variable_has_no_value ->
        Printf.printf "Variable has no value\n";
        exit 1
    | Failure msg ->
        Printf.printf "Runtime error: %s\n" msg;
        exit 1
    | exn ->
        Printf.printf "Unknown error: %s\n" (Printexc.to_string exn);
        exit 1

(* let () =
  Printf.printf "Enter input:\n";
  flush stdout;
  parse_and_eval_input () *)



let () =
  let filename =
    if Array.length Sys.argv > 1 then Sys.argv.(1)
    else (Printf.eprintf "Usage: %s <input_file.dsl>\n" Sys.argv.(0); exit 1)
  in
  let chan = open_in filename in
  let lexbuf = Lexing.from_channel chan in
  try
    let ast = Parser.start_symbol Lexer.lexer lexbuf in
    Printf.printf "Parsed AST:\n%s\n" (show_start_symbol ast);
    match ast with
    | Control c_list ->
        Eval.eval_control (Seq c_list)
  with
  | End_of_file ->
      close_in chan
  | Parsing.Parse_error ->
      let pos = Lexing.lexeme_start_p lexbuf in
      Printf.printf "Syntax error at line %d, column %d\n"
        pos.pos_lnum (pos.pos_cnum - pos.pos_bol);
      close_in chan;
      exit 1
  | Types.TypeError ->
      Printf.printf "Type error during evaluation\n";
      close_in chan;
      exit 1
  | Types.Variable_has_no_value ->
      Printf.printf "Variable has no value\n";
      close_in chan;
      exit 1
  | Failure msg ->
      Printf.printf "Runtime error: %s\n" msg;
      close_in chan;
      exit 1
  | exn ->
      Printf.printf "Unknown error: %s\n" (Printexc.to_string exn);
      close_in chan;
      exit 1
