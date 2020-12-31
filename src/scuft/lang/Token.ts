export class Token {
    stringValue: string; // string of token in written program

    line: number;
    c: number; // horizontal pozitional accross line

    type: TokenType;
    typeName: string;

    value!: number | string;

    constructor(stringValue: string, line: number, c: number, type: TokenType) {
        this.stringValue = stringValue;

        this.line = line;
        this.c = c;

        this.type = type;
        this.typeName = TokenType[type];
    }
    equals(other: Token): boolean {
        return other.line === this.line && other.c === this.c && this.typeEquals(other) && other.value === this.value;
    }
    typeEquals(other: Token): boolean {
        return other.type === this.type;
    }
}

export enum TokenType {
    // Statements
    IDENTIFIER_TKN,
    COLON_TKN,
    NUMERIC_LITERAL_TKN,
    STRING_LITERAL_TKN,

    //Assignments
    ASSIGNMENT_TKN,
    CONST_ASSIGNMENT_TKN,
    MUTABLE_ASSIGNMENT_TKN,

    // Blocks
    LEFT_CURLY_TKN,
    RIGHT_CURLY_TKN,
    LEFT_SQUARE_TKN,
    RIGHT_SQUARE_TKN,
    LEFT_PARENS_TKN,
    RIGHT_PARENS_TKN,

    // Statement Keywords
    TYPE_TKN, // particle = type {} or typeof(particle) == type;
    MODULE_TKN,
    WITH_TKN,
    IF_TKN,
    ELSE_TKN,
    WHILE_TKN,
    FOR_TKN,
    IN_TKN,
    BREAK_TKN,
    CONTINUE_TKN,

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
    VOID_TYPE_TKN,
    NUM_TYPE_TKN,
    STRING_TYPE_TKN,
    BOOL_TYPE_TKN,

    // Bitwise Operators
    BIN_OR_TKN, // or
    BIN_AND_TKN, // and
    BIN_XOR_TKN, // xor
    BIN_NOT_TKN, // ~
    BIN_SHIFT_RIGHT_TKN, // >>
    BIN_SHIFT_ARITHMETIC_RIGHT_TKN, // >>>
    BIN_SHIFT_LEFT_TKN, // <<

    // Function Defs
    ARROW_TKN, // () "->" void
    RETURN_TKN,

    // Lambda Def and function type
    BACKSLASH_TKN,

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

    // Directives
    HASH_RANGE_TKN,

    // Miscellaneous
    DOUBLE_QUOTE_TKN,
    DOT_TKN,
    ELLIPSIS_TKN, // for [array] doSomething(...)
    REVERSE_ARROW_TKN, // Particle.{ x <- 4, y <- 3}
    COMMA_TKN,
    SEMI_COLON_TKN,
    UNKNOWN_TKN,
    END_TKN,
}
