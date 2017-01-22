module Token (Token (..))where
-- The token type:

data Token =
    TVAR      |
    TWHILE    |
    TDO       |
    TDONE     |
    TFLOAT    |
    TINT      |
    TPRINT    |
    TREAD     |
    TSTRING   |
    TIF       |
    TELSE     |
    TTHEN     |
    TENDIF    |
    TADD      |
    TSUB      |
    TMULT     |
    TDIV      |
    TEQUALS   |
    T_OPAREN  |
    T_CPAREN  |
    T_OBRACE  |
    T_CBRACE  |
    T_DQUOTE  |
    T_SCOLON  |
    T_COLON   |
    T_ID_VAL String      |
    T_INT_VAL Integer    |
    T_FLOAT_VAL Float    |
    T_STRING_VAL String 
    deriving (Eq,Show)
