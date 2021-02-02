export class Token {
    stringValue: string; // string of token in written program

    line: number;
    c: number; // horizontal pozitional accross line

    type: TokenType;
    typeName: string;

    value!: number | string;

    static fromType(type: TokenType, locToken: Token): Token {
        return new Token(tokenToStr(type), locToken.line, locToken.c, type);
    }
    constructor(stringValue: string, line: number, c: number, type: TokenType) {
        this.stringValue = stringValue;

        this.line = line;
        this.c = c;

        this.type = type;
        this.typeName = TokenType[type];
    }
    equals(other: Token): boolean {
        return (
            other.line === this.line && other.c === this.c && this.isTokenTypeEqual(other) && other.value === this.value
        );
    }
    isTokenTypeEqual(other: Token): boolean {
        return other.type === this.type;
    }
    // Returns whether the otherAfterCurrent token is after the current
    isTokenBefore(otherAfterCurrent: Token): boolean {
        return (
            this.line < otherAfterCurrent.line || (this.line === otherAfterCurrent.line && this.c < otherAfterCurrent.c)
        );
    }
}

export function tokenToStr(tokenType: TokenType): string {
    switch (tokenType) {
        case TokenType.COLON_TKN:
            return ":";

        //Assignments
        case TokenType.CONST_ASSIGNMENT_TKN:
            return "=>";
        case TokenType.IMMUTABLE_ASSIGNMENT_TKN:
            return "=";
        case TokenType.MUTABLE_ASSIGNMENT_TKN:
            return "~=";

        // Blocks
        case TokenType.LEFT_CURLY_TKN:
            return "{";
        case TokenType.RIGHT_CURLY_TKN:
            return "}";
        case TokenType.LEFT_SQUARE_TKN:
            return "[";
        case TokenType.RIGHT_SQUARE_TKN:
            return "]";
        case TokenType.LEFT_PARENS_TKN:
            return "(";
        case TokenType.RIGHT_PARENS_TKN:
            return ")";

        // Statement Keywords
        case TokenType.TYPE_TKN:
            return "type";
        case TokenType.MODULE_TKN:
            return "module";
        case TokenType.IF_TKN:
            return "if";
        case TokenType.ELSE_TKN:
            return "else";
        case TokenType.WHILE_TKN:
            return "while";
        case TokenType.FOR_TKN:
            return "for";
        case TokenType.IN_TKN:
            return "in";
        case TokenType.BREAK_TKN:
            return "break";
        case TokenType.CONTINUE_TKN:
            return "continue";

        // Conditionals
        case TokenType.COND_OR_TKN:
            return "||";
        case TokenType.COND_AND_TKN:
            return "&&";
        case TokenType.COND_XOR_TKN:
            return "$$";
        case TokenType.COND_NOT_TKN:
            return "!";
        case TokenType.COND_EQUALS_TKN:
            return "==";
        case TokenType.COND_NOT_EQUALS_TKN:
            return "!=";
        case TokenType.COND_LESS_THAN_TKN:
            return "<";
        case TokenType.COND_LESS_THAN_EQUAL_TKN:
            return "<=";
        case TokenType.COND_GREATER_THAN_TKN:
            return ">";
        case TokenType.COND_GREATER_THAN_EQUAL_TKN:
            return ">=";
        case TokenType.COND_TRUE_TKN:
            return "true";
        case TokenType.COND_FALSE_TKN:
            return "false";

        // Types
        case TokenType.MUT_CAST_TKN:
            return "mut";
        case TokenType.IMMUT_CAST_TKN:
            return "immut";
        case TokenType.CONST_CAST_TKN:
            return "const";
        case TokenType.VOID_TYPE_TKN:
            return "void";
        case TokenType.NUM_TYPE_TKN:
            return "num";
        case TokenType.STRING_TYPE_TKN:
            return "string";
        case TokenType.BOOL_TYPE_TKN:
            return "bool";

        // Bitwise Operators
        case TokenType.BIN_OR_TKN:
            return "or";
        case TokenType.BIN_AND_TKN:
            return "and";
        case TokenType.BIN_XOR_TKN:
            return "xor";
        case TokenType.BIN_NOT_TKN:
            return "~";
        case TokenType.BIN_SHIFT_RIGHT_TKN:
            return ">>";
        case TokenType.BIN_SHIFT_ARITHMETIC_RIGHT_TKN:
            return ">>>";
        case TokenType.BIN_SHIFT_LEFT_TKN:
            return "<<";

        // Function Defs
        case TokenType.ARROW_TKN:
            return "->";
        case TokenType.RETURN_TKN:
            return "return";

        // Lambda Def and function type
        case TokenType.BACKSLASH_TKN:
            return "\\";

        // Operators
        case TokenType.OP_ADD_TKN:
            return "+";
        case TokenType.OP_SUBTR_TKN:
            return "-";
        case TokenType.OP_MULT_TKN:
            return "*";
        case TokenType.OP_DIVIDE_TKN:
            return "/";
        case TokenType.OP_CARROT_TKN:
            return "^";
        case TokenType.OP_MODULUS_TKN:
            return "%";
        case TokenType.OP_ADD_ADD_TKN:
            return "++";
        case TokenType.OP_ADD_EQUALS_TKN:
            return "+=";
        case TokenType.OP_SUBTR_SUBTR_TKN:
            return "--";
        case TokenType.OP_SUBTR_EQUALS_TKN:
            return "-=";
        case TokenType.OP_MULT_EQUALS_TKN:
            return "*=";
        case TokenType.OP_DIV_EQUALS_TKN:
            return "/=";

        // Miscellaneous
        case TokenType.DOUBLE_QUOTE_TKN:
            return '"';
        case TokenType.DOT_TKN:
            return ".";
        case TokenType.REVERSE_ARROW_TKN:
            return "<-";
        case TokenType.COMMA_TKN:
            return ",";
        case TokenType.SEMI_COLON_TKN:
            return ";";
        default:
            return TokenType[tokenType];
    }
}

export enum TokenType {
    IDENTIFIER_TKN,
    COLON_TKN,
    NUMERIC_LITERAL_TKN,
    STRING_LITERAL_TKN,

    //Assignments
    IMMUTABLE_ASSIGNMENT_TKN,
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
    MUT_CAST_TKN,
    IMMUT_CAST_TKN, // For the sake of completion
    CONST_CAST_TKN,
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
    OP_ADD_ADD_TKN,
    OP_ADD_EQUALS_TKN,
    OP_SUBTR_SUBTR_TKN,
    OP_SUBTR_EQUALS_TKN,
    OP_MULT_EQUALS_TKN,
    OP_DIV_EQUALS_TKN,

    // Miscellaneous
    DOUBLE_QUOTE_TKN,
    DOT_TKN,
    REVERSE_ARROW_TKN, // Particle.{ x <- 4, y <- 3}
    COMMA_TKN,
    SEMI_COLON_TKN,
    UNKNOWN_TKN,
    END_TKN,
}
