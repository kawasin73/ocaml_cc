%token EOF 
%token <int> NUM
%token <string> ID
%token BREAK CONTINUE
%token ELSE IF
%token RETURN
%token WHILE
%token LONG
%token LPAREN RPAREN
%token LBRACE RBRACE
%token SEMICOLON
%token COMMA
%token EQ
%token EQEQ NEQ
%token LT GT LEQ GEQ
%token PLUS MINUS
%token ASTERISK
%token DIV MOD
%token BANG

%right EQ
%left EQEQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS
%left ASTERISK DIV MOD
%nonassoc BANG

%{
%}

%start program
%type <Cc_ast.definition list> program

%% 
program :
 | definition* EOF                 { $1 }

definition :
 | fun_definition                  { $1 }

fun_definition :
 | type_expr ID LPAREN parameter_list RPAREN compound_stmt { Cc_ast.FUN_DEF($1, $2, $4, $6) }

parameter_list :
 |                                 { [] }
 | parameter comma_parameter*      { $1::$2 }

comma_parameter :     
 | COMMA parameter                 { $2 }

parameter :
 | type_expr ID                    { ($1,$2) }

type_expr :
 | LONG                            { Cc_ast.TYPE_LONG }

stmt :     
 | SEMICOLON                       { Cc_ast.STMT_EMPTY }
 | CONTINUE SEMICOLON              { Cc_ast.STMT_CONTINUE }
 | BREAK SEMICOLON                 { Cc_ast.STMT_BREAK }
 | RETURN expr SEMICOLON           { Cc_ast.STMT_RETURN($2) }
 | compound_stmt                   { $1 }
 | while_stmt                      { $1 }
 | if_stmt                         { $1 }
 | expr SEMICOLON                  { Cc_ast.STMT_EXPR($1) }

compound_stmt :
 | LBRACE var_decl* stmt* RBRACE   { Cc_ast.STMT_COMPOUND($2, $3) }

var_decl :
 | type_expr ID SEMICOLON          { ($1, $2) }

if_stmt :
 | IF LPAREN expr RPAREN stmt ELSE stmt { Cc_ast.STMT_IF($3, $5, $7) }
 | IF LPAREN expr RPAREN stmt           { Cc_ast.STMT_IF($3, $5, Cc_ast.STMT_EMPTY) }
     
while_stmt :
 | WHILE LPAREN expr RPAREN stmt   { Cc_ast.STMT_WHILE($3, $5) }

expr :
 | NUM                       { Cc_ast.EXPR_NUM($1) }
 | ID                        { Cc_ast.EXPR_VAR($1) }
 | ID LPAREN arg_list RPAREN { Cc_ast.EXPR_CALL($1, $3) }
 | LPAREN expr RPAREN        { $2 }
 | PLUS expr                 { Cc_ast.EXPR_UN_OP(Cc_ast.UN_OP_PLUS,  $2) }
 | MINUS expr                { Cc_ast.EXPR_UN_OP(Cc_ast.UN_OP_MINUS, $2) }
 | BANG expr                 { Cc_ast.EXPR_UN_OP(Cc_ast.UN_OP_BANG,  $2) }
 | expr EQ expr       { Cc_ast.EXPR_BIN_OP(Cc_ast.BIN_OP_EQ,    $1, $3) }
 | expr EQEQ expr     { Cc_ast.EXPR_BIN_OP(Cc_ast.BIN_OP_EQEQ,  $1, $3) }
 | expr NEQ expr      { Cc_ast.EXPR_BIN_OP(Cc_ast.BIN_OP_NEQ,   $1, $3) }
 | expr LT expr       { Cc_ast.EXPR_BIN_OP(Cc_ast.BIN_OP_LT,    $1, $3) }
 | expr GT expr       { Cc_ast.EXPR_BIN_OP(Cc_ast.BIN_OP_GT,    $1, $3) }
 | expr LEQ expr      { Cc_ast.EXPR_BIN_OP(Cc_ast.BIN_OP_LEQ,   $1, $3) }
 | expr GEQ expr      { Cc_ast.EXPR_BIN_OP(Cc_ast.BIN_OP_GEQ,   $1, $3) }
 | expr PLUS expr     { Cc_ast.EXPR_BIN_OP(Cc_ast.BIN_OP_PLUS,  $1, $3) }
 | expr MINUS expr    { Cc_ast.EXPR_BIN_OP(Cc_ast.BIN_OP_MINUS, $1, $3) }
 | expr ASTERISK expr { Cc_ast.EXPR_BIN_OP(Cc_ast.BIN_OP_MUL,   $1, $3) }
 | expr DIV expr      { Cc_ast.EXPR_BIN_OP(Cc_ast.BIN_OP_DIV,   $1, $3) }
 | expr MOD expr      { Cc_ast.EXPR_BIN_OP(Cc_ast.BIN_OP_MOD,   $1, $3) }

arg_list :
 |                                 { [] }
 | expr comma_expr*                { $1::$2 }

comma_expr :
 | COMMA expr                      { $2 }

%% 
