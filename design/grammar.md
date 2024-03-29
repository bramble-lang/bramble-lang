```
Grammar
PRIMITIVE := i32 | bool
IDENTIFIER := A-Za-z*
ID_DEC := IDENTIFIER COLON PRIMITIVE
NUMBER := 0-9*
FUNCTION_CALL := IDENTIFIER LPAREN EXPRESSION [, EXPRESSION]* RPAREN
YIELD := yield IDENTIFIER
IF := if EXPRESSION LBRACE EXPRESSION RBRACE else LBRACE EXPRESSION RBRACE
FACTOR := FUNCTION_CALL | YIELD | NUMBER | IDENTIFIER | IF
TERM := FACTOR [* TERM]
EXPRESSION_BLOCK := {STATEMENT* [EXPRESSION]}
EXPRESSION :=  TERM [+ EXPRESSION] | EXPESSION_BLOCK
INIT_CO := init IDENTIFIER
ASSIGN := IDENTIFIER = EXPRESSION;
BIND := let [mut] ID_DEC := (EXPRESSION|INIT_CO)
PRINTLN := println EXPRESSION ;
RETURN := return [EXPRESSION] SEMICOLON
YIELD_RETURN := yield return [EXPRESSION] SEMICOLON
STATEMENT := [BIND] SEMICOLON
BLOCK := STATEMENT*
COBLOCK := [STATEMENT | YIELD_RETURN]*
FUNCTION := fn IDENTIFIER LPAREN [ID_DEC [, ID_DEC]*] RPAREN  [LARROW PRIMITIVE] LBRACE BLOCK RETURN RBRACE
COROUTINE := co IDENTIFIER LPAREN [ID_DEC [, ID_DEC]*] RPAREN [LARROW PRIMITIVE] LBRACE COBLOCK RETURN RBRACE
STRUCT_INIT := IDENTIFIER LBRACE [IDENTIFIER : PRIMITIVE]* RBRACE
STRUCT_DEF := struct IDENTIFIER LBRACE [ID_DEC]* RBRACE
MODULES := [FUNCTION|COROUTINE|STRUCT]*

tokenize - takes a string of text and converts it to a string of tokens
parse - takes a string of tokens and converts it into an AST
compile - takes an AST and converts it to assembly
```