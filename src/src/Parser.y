{
module Parser (parseInput) where
import Token (Token (..))
}

%name parseInput
%tokentype { Token }
%error { parseError }


%token
-- Types
    int { TINT }
    float { TFLOAT }
    string  { TSTRING }
    var    { TVAR }
    while  { TWHILE }
    do { TDO }
    done { TDONE }
    print { TPRINT }
    read { TREAD }
    if { TIF }
    else { TELSE }
    then { TTHEN }
    endif { TENDIF }
    '+' { TADD }
    '-' { TSUB }
    '*' { TMULT }
    '/' { TDIV }
    '=' { TEQUALS }
    '(' { T_OPAREN }
    ')' { T_CPAREN }
    '{' { T_OBRACE }
    '}' { T_CBRACE }
    '"' { T_DQUOTE }
    ';' { T_SCOLON }
    ':' { T_COLON }
    id_val { T_ID_VAL $$ }
    int_val { T_INT_VAL $$ }
    float_val { T_FLOAT_VAL $$ }
    string_val { T_STRING_VAL $$ }

%left '+' '-'
%left '*' '/'
%left UMINUS

%%

Program : Declarations Statements {$1 && $2}

Declarations : Declaration Declarations {$1 && $2}
      | {- empty -} {True}

Statements : Statement Statements {$1 && $2}
      | {- empty -} {True}

VariableType : int { True }
     	     | float { True }
       	     | string { True }

Declaration : var id_val ':' VariableType ';' { $4 }

Statement : read id_val ';' { True }
	  | print Expression ';' { $2 }
	  | id_val '=' Expression ';' { $3 }
	  | if Expression then Statements ElseBlockFactor { $2 && $4 && $5 }
	  | while Expression do Statements done { $2 && $4 }

ElseBlockFactor : endif { True }
	        | else Statements endif {$2}

Expression : '(' Expression ')' { $2 }
	   | '-' Expression %prec UMINUS { $2 }
	   | Expression '+' Expression { $1 && $3 }
	   | Expression '-' Expression { $1 && $3 }
	   | Expression '*' Expression { $1 && $3 }
	   | Expression '/' Expression { $1 && $3 }
	   | int_val { True }
	   | float_val { True }
	   | id_val { True }
	   | string_val { True }
{
parseError :: [Token] -> a
parseError _ = error "Parse Error"
}
