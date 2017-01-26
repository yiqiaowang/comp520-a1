{
module Scanner(scanToken) where
import Token (Token (..))
}

%wrapper "basic"

-- Character Set Macros
$letter = [a-zA-Z]
$number_zero = [0-9]
$number_nonzero = [1-9]
$whitespace =[\ \t\r]

-- reserved words
@var = var
@while = while
@do = do
@done = done
@float = float
@int = int
@print = print
@read = read
@string = string
@if = if
@then = then
@endif = endif
@else = else

-- other
@underscore = "_"
@semi_colon = ";"
@equals = "="
@colon = ":"
@plus = \+
@mult = \*
@sub = \-
@div = \/
@o_paren = \(
@c_paren = \)
@o_brace = \{
@c_brace = \}
@d_quote = \"
@new_line = \n
@forward_slash = \/
@back_slash = \\

-- tokens macros

@id_first = ($letter|@underscore)
@id_rest = ($letter|@underscore|$number_zero)
@id_val = (@id_first|@id_first@id_rest+)

@int_val = ($number_zero|$number_nonzero$number_zero+)

-- floating point [0-9]\.[0-9]*

@float_less_one = ($number_zero\.$number_zero+)
@float_greater_one = ($number_nonzero$number_zero*\.$number_zero+)
@float_val = (@float_less_one|@float_greater_one)

@escape = (a|b|f|n|r|t|v|@back_slash|'|@d_quote)

@string_val = @d_quote([^"\\]|\\@escape)*@d_quote


-- Comment  \\\\[^\n]*
@comment_start = @forward_slash{2}
$comment_content = [~\n]
@comment_val = @comment_start$comment_content*

tokens :-

-- Keywords Start
@var    {\s -> TVAR}
@while  {\s -> TWHILE}
@do     {\s -> TDO}
@done	{\s -> TDONE}
@float  {\s -> TFLOAT}
@int    {\s -> TINT}
@print  {\s -> TPRINT}
@read   {\s -> TREAD}
@string {\s -> TSTRING}
@if     {\s -> TIF}
@then	{\s -> TTHEN}
@endif  {\s -> TENDIF}
@else	{\s -> TELSE}
-- Keywords End

-- Tokens in the following section do not require any additional text
@plus  {\s -> TADD}
@mult  {\s -> TMULT}
@sub   {\s -> TSUB}
@div   {\s -> TDIV}
@equals {\s -> TEQUALS}
@o_brace  {\s -> T_OBRACE}
@c_brace  {\s -> T_CBRACE}
@o_paren  {\s -> T_OPAREN}
@c_paren  {\s -> T_CPAREN}
@d_quote  {\s -> T_DQUOTE}
@semi_colon {\s -> T_SCOLON}
@colon {\s -> T_COLON}

-- Every token after this one holds some value
@id_val     {\s -> T_ID_VAL s}
@int_val {\s -> T_INT_VAL (read s)}
@float_val  {\s -> T_FLOAT_VAL (read s)}
@string_val {\s -> T_STRING_VAL s}


-- Whitespace, newlines, comments
$whitespace+ ;
@new_line ;
@comment_val ;

{
scanToken :: String -> [Token]
scanToken [] = []
scanToken s = alexScanTokens s
}
