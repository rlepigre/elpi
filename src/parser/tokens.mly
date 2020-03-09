%token FULLSTOP
%token < string > CONSTANT
%token VDASH
%token QDASH
%token < int > INTEGER
%token < float > FLOAT
%token < string > STRING
%token FRESHUV
%token CUT
%token COLON
%token BIND
%token LPAREN
%token RPAREN
%token LBRACKET
%token RBRACKET
%token LCURLY
%token RCURLY
%token PIPE
%token AS
%token IS
%token <char> IO
%token ARROW
%token DARROW
%token DIV
%token MOD
%token < string > QUOTED
%token SHORTEN
%token ACCUMULATE
%token LOCAL
%token PRED
%token MINUS
%token MINUSr
%token MINUSi
%token MINUSs
%token MODE
%token MACRO
%token RULE
%token NAMESPACE
%token CONSTRAINT
%token KIND
%token TYPE
%token TYPEABBREV
%token EXTERNAL
%token MODULE
%token SIG
%token IMPORT
%token ACCUM_SIG
%token USE_SIG
%token LOCALKIND
%token USEONLY
%token EXPORTDEF
%token CLOSED
%token <string> FIXITY
%token PI
%token SIGMA
%token IF
%token BEFORE
%token AFTER
%token NAME 
%token INDEX 
%token CONS
%token CONJ
%token OR
%token EQ
%token EQ2
%token IFF
%token NIL
%token EOF

%token <string> SYMB_PLUS
%token <string> SYMB_TIMES
%token <string> SYMB_MINUS
%token <string> SYMB_EXP
%token <string> SYMB_LT
%token <string> SYMB_GT
%token <string> SYMB_EQ
%token <string> SYMB_QMARK
%token <string> SYMB_BTICK
%token <string> SYMB_TICK
%token <string> SYMB_SHARP
%token <string> SYMB_TILDE
%token <string> SYMB_AND
%token <string> SYMB_OR
%token <string> SYMB_SLASH

%%