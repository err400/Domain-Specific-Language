# ocamlc -c types.ml
# ocamlc -c ast.ml
# ocamlyacc parser.mly
# ocamlc -c parser.mli
# ocamlc -c parser.ml 
# ocamllex lexer.mll
# ocamlc -c lexer.ml 
# ocamlc -c assignment_3.ml
# ocamlc -o test_lexer types.cmo ast.cmo parser.cmo lexer.cmo assignment_3.cmo
# ocamlc -c main.ml
# ocamlc -o main types.cmo ast.cmo parser.cmo lexer.cmo main.cmo

OCAMLC = ocamlc
OCAMLYACC = ocamlyacc
OCAMLEX = ocamllex

TYPES = types.ml
AST = ast.ml
PARSER = parser.mly
LEXER = lexer.mll
ASSIGNMENT = assignment_3.ml
MAIN = main.ml
EVAL = eval.ml
FUNCTIONS = functions.ml

TYPES_O = types.cmo
AST_O = ast.cmo
PARSER_MLI = parser.mli
PARSER_ML = parser.ml
LEXER_ML = lexer.ml
ASSIGNMENT_CMO = assignment_3.cmo
MAIN_CMO = main.cmo
PARSER_CMO = parser.cmo
LEXER_CMO = lexer.cmo
EVAL_CMO = eval.cmo
FUNCTIONS_CMO = functions.cmo

# all: test_lexer main

all: main

test_lexer: $(TYPES_O) $(AST_O) $(PARSER_CMO) $(LEXER_CMO) $(ASSIGNMENT_CMO)
	@$(OCAMLC) -o $@ $^

main: $(TYPES_O) $(AST_O) $(PARSER_CMO) $(LEXER_CMO) $(FUNCTIONS_CMO) $(EVAL_CMO) $(MAIN_CMO)
	@$(OCAMLC) -o $@ $^

$(TYPES_O): $(TYPES)
	@$(OCAMLC) -c $<

$(AST_O): $(AST)
	@$(OCAMLC) -c $<

$(PARSER_MLI) $(PARSER_ML): $(PARSER)
	@$(OCAMLYACC) $<

parser.cmi: $(PARSER_MLI)
	@$(OCAMLC) -c $<

$(PARSER_CMO): parser.cmi $(PARSER_ML)
	@$(OCAMLC) -c $(PARSER_ML)

$(LEXER_ML): $(LEXER)
	@$(OCAMLEX) $<

$(LEXER_CMO): $(LEXER_ML)
	@$(OCAMLC) -c $<

$(ASSIGNMENT_CMO): $(ASSIGNMENT)
	@$(OCAMLC) -c $<

$(FUNCTIONS_CMO): $(FUNCTIONS)
	@$(OCAMLC) -c $<

$(EVAL_CMO): $(EVAL)
	@$(OCAMLC) -c $<

$(MAIN_CMO): $(MAIN)
	@$(OCAMLC) -c $<

clean:
	@rm -f *.cmo *.cmi *.out $(TYPES_O) $(AST_O) $(PARSER_MLI) $(PARSER_ML) $(LEXER_ML) $(ASSIGNMENT_CMO) $(MAIN_CMO) $(PARSER_CMO) $(LEXER_CMO) $(EVAL_CMO) $(FUNCTIONS_CMO) main
