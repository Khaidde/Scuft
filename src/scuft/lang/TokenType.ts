enum TokenType {
    // Statements
    IDENTIFIER_TKN,
    ASSIGNMENT_TKN,
    COLON_TKN,
    NUMERIC_LITERAL_TKN,
    STRING_LITERAL_TKN,

    // Blocks
    LEFT_CURLY_TKN,
    RIGHT_CURLY_TKN,
    LEFT_SQUARE_TKN,
    RIGHT_SQUARE_TKN,
    LEFT_PARENS_TKN,
    RIGHT_PARENS_TKN,

    // Keywords
    FOR_TKN,
    WHILE_TKN,
    BREAK_TKN,
    CONTINUE_TKN,
    IF_TKN,
    ELSE_TKN,
    TYPE_TKN,
    MODULE_TKN,

    // Conditionals
    COND_OR_TKN, // ||
    COND_AND_TKN, // &&
    COND_XOR_TKN, // $$
    COND_NOT_TKN, // !
    COND_EQUALS_TKN,
    COND_NOT_EQUALS_TKN,
    COND_LESS_THAN_TKN,
    COND_LESS_THAN_EQUAL_TKN,
    COND_GREATER_THAN_TKN,
    COND_GREATER_THAN_EQUAL_TKN,
    COND_TRUE_TKN,
    COND_FALSE_TKN,

    // Types
    VOID_TKN,
    NUM_TKN,
    STRING_TKN,
    BOOL_TKN,

    // Bitwise Operators
    BIN_OR_TKN, // or
    BIN_AND_TKN, // and
    BIN_XOR_TKN, // xor
    BIN_NOT_TKN, // ~
    BIN_SHIFT_RIGHT_TKN, // >>
    BIN_SHIFT_ARITHMETIC_RIGHT_TKN, // >>>
    BIN_SHIFT_LEFT_TKN, // <<

    // Func Defs
    FUNC_MAPPING_TKN, // () "->" void
    RETURN_TKN,

    // Operators
    OP_ADD_TKN,
    OP_SUBTR_TKN,
    OP_MULT_TKN,
    OP_DIVIDE_TKN,
    OP_CARROT_TKN,
    OP_MODULUS_TKN,
    OP_BAR_TKN,
    OP_ADD_ADD_TKN,
    OP_ADD_EQUALS_TKN,
    OP_SUBTR_SUBTR_TKN,
    OP_SUBTR_EQUALS_TKN,
    OP_MULT_EQUALS_TKN,
    OP_DIV_EQUALS_TKN,

    //Miscellaneous
    DOUBLE_QUOTE_TKN,
    DOT_TKN,
    ELLIPSES_TKN, // for [array] doSomething(...)
    COMMA_TKN,
    SEMI_COLON_TKN,
    UNKNOWN_TKN,
    END_TKN,
}

export default TokenType;
