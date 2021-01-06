import { ErrorHandler } from "./ErrorHandler";
import { Token, TokenType, tokenToStr } from "./Token";

export default class Lexer {
    private readonly err: ErrorHandler;
    private readonly sourceCode: string;
    private curIndex = 0;
    private curLine = 1;
    private curC = 0;
    constructor(sourceCode: string, errHandler: ErrorHandler) {
        this.sourceCode = sourceCode;
        this.err = ErrorHandler.fromHandler("----Lexer----\n", errHandler);
    }
    lex(): Token[] {
        let tokens: Token[] = [];
        let token = this.nextToken();
        while (token.type !== TokenType.END_TKN) {
            tokens.push(token);
            token = this.nextToken();
        }
        tokens.push(token);
        return tokens;
    }
    private getChar(index: number): string {
        return this.sourceCode.charAt(index);
    }
    private getCurChar(): string {
        return this.getChar(this.curIndex);
    }
    private getNextChar(): string {
        return this.getChar(this.curIndex + 1);
    }
    private incrementCurIndex(amount: number) {
        if (this.getCurChar() === "\n") {
            this.curLine++;
            this.curC = -1;
        }
        this.curIndex += amount;
        this.curC += amount;
    }
    nextToken(): Token {
        //Clear Whitespace
        while (Lexer.isWhitespace(this.getCurChar())) {
            this.incrementCurIndex(1);
        }

        //Return end token if end of string input reached
        if (this.curIndex >= this.sourceCode.length) {
            return this.grabToken("' '", TokenType.END_TKN);
        }

        switch (this.getCurChar()) {
            //Statements
            case ">":
                if (this.getNextChar() === "=") {
                    return this.grabToken(">=", TokenType.COND_GREATER_THAN_EQUAL_TKN);
                } else if (this.getNextChar() === ">" && this.getChar(this.curIndex + 2) === ">") {
                    return this.grabToken(">>>", TokenType.BIN_SHIFT_ARITHMETIC_RIGHT_TKN);
                } else if (this.getNextChar() === ">") {
                    return this.grabToken(">>", TokenType.BIN_SHIFT_ARITHMETIC_RIGHT_TKN);
                } else {
                    return this.grabToken(">", TokenType.COND_GREATER_THAN_TKN);
                }
            case "<":
                switch (this.getNextChar()) {
                    case "=":
                        return this.grabToken("<=", TokenType.COND_LESS_THAN_EQUAL_TKN);
                    case "<":
                        return this.grabToken("<<", TokenType.BIN_SHIFT_LEFT_TKN);
                    case "-":
                        return this.grabToken("<-", TokenType.REVERSE_ARROW_TKN);
                    default:
                        return this.grabToken("<", TokenType.COND_LESS_THAN_TKN);
                }
            case ":":
                return this.grabToken(":", TokenType.COLON_TKN);

            // Assignment
            case "=":
                switch (this.getNextChar()) {
                    case "=":
                        return this.grabToken("==", TokenType.COND_EQUALS_TKN);
                    case ">":
                        return this.grabToken("=>", TokenType.CONST_ASSIGNMENT_TKN);
                    default:
                        return this.grabToken("=", TokenType.IMMUTABLE_ASSIGNMENT_TKN);
                }

            // Blocks
            case "{":
                return this.grabToken("{", TokenType.LEFT_CURLY_TKN);
            case "}":
                return this.grabToken("}", TokenType.RIGHT_CURLY_TKN);
            case "[":
                return this.grabToken("[", TokenType.LEFT_SQUARE_TKN);
            case "]":
                return this.grabToken("]", TokenType.RIGHT_SQUARE_TKN);
            case "(":
                return this.grabToken("(", TokenType.LEFT_PARENS_TKN);
            case ")":
                return this.grabToken(")", TokenType.RIGHT_PARENS_TKN);

            //Conditionals
            case "|":
                if (this.getNextChar() === "|") {
                    return this.grabToken("||", TokenType.COND_OR_TKN);
                } else {
                    return this.grabToken("|", TokenType.BIN_OR_TKN);
                }
            case "&":
                if (this.getNextChar() === "&") {
                    return this.grabToken("&&", TokenType.COND_AND_TKN);
                } else {
                    return this.grabToken("&", TokenType.BIN_AND_TKN);
                }
            case "$":
                if (this.getNextChar() === "$") {
                    return this.grabToken("$$", TokenType.COND_XOR_TKN);
                } else {
                    return this.grabToken("$", TokenType.BIN_XOR_TKN);
                }
            case "!":
                if (this.getNextChar() === "=") {
                    return this.grabToken("!=", TokenType.COND_NOT_EQUALS_TKN);
                } else {
                    return this.grabToken("!", TokenType.COND_NOT_TKN);
                }

            // Operators
            case "+":
                switch (this.getNextChar()) {
                    case "=":
                        return this.grabToken("+=", TokenType.OP_ADD_EQUALS_TKN);
                    case "+":
                        return this.grabToken("++", TokenType.OP_ADD_ADD_TKN);
                    default:
                        return this.grabToken("+", TokenType.OP_ADD_TKN);
                }
            case "-":
                switch (this.getNextChar()) {
                    case "=":
                        return this.grabToken("-=", TokenType.OP_SUBTR_EQUALS_TKN);
                    case "-":
                        return this.grabToken("--", TokenType.OP_SUBTR_SUBTR_TKN);
                    case ">":
                        return this.grabToken("->", TokenType.ARROW_TKN);
                    default:
                        return this.grabToken("-", TokenType.OP_SUBTR_TKN);
                }
            case "*":
                switch (this.getNextChar()) {
                    case "=":
                        return this.grabToken("*=", TokenType.OP_MULT_EQUALS_TKN);
                    case "/":
                        this.err.atPoint_PANIC("Unclosed block comment", this.curLine, this.curC);
                    default:
                        return this.grabToken("*", TokenType.OP_MULT_TKN);
                }
            case "/":
                switch (this.getNextChar()) {
                    case "/":
                        this.consumeSingleLineComment();
                        return this.nextToken();
                    case "*":
                        this.consumeBlockComment();
                        return this.nextToken();
                    case "=":
                        return this.grabToken("/=", TokenType.OP_DIV_EQUALS_TKN);
                    default:
                        return this.grabToken("/", TokenType.OP_DIVIDE_TKN);
                }
            case "^":
                return this.grabToken("^", TokenType.OP_CARROT_TKN);
            case "%":
                return this.grabToken("%", TokenType.OP_MODULUS_TKN);

            //Miscellaneous
            case "~":
                if (this.getNextChar() == "=") {
                    return this.grabToken("~=", TokenType.MUTABLE_ASSIGNMENT_TKN);
                } else {
                    return this.grabToken("~", TokenType.BIN_NOT_TKN);
                }
            case ".":
                if (this.getNextChar() === "." && this.getChar(this.curIndex + 2) === ".") {
                    return this.grabToken("...", TokenType.ELLIPSIS_TKN);
                } else {
                    return this.grabToken(".", TokenType.DOT_TKN);
                }
            case "\\":
                return this.grabToken("\\", TokenType.BACKSLASH_TKN);
            case ",":
                return this.grabToken(",", TokenType.COMMA_TKN);
            case ";":
                return this.grabToken(";", TokenType.SEMI_COLON_TKN);
            case '"':
                return this.grabStringLiteral();
            default:
                if (Lexer.isLetter(this.getCurChar())) {
                    return this.grabIdentifier();
                } else if (Lexer.isNumber(this.getCurChar())) {
                    return this.grabNumericLiteral();
                } else if (this.getCurChar() === "#") {
                    return this.grabDirective();
                } else {
                    let unknown = this.grabToken(this.getCurChar(), TokenType.UNKNOWN_TKN);
                    unknown.value = this.getCurChar();
                    return unknown;
                }
        }
    }
    private grabToken(str: string, type: TokenType): Token {
        this.incrementCurIndex(str.length);
        return this.makeToken(str, type);
    }
    private makeToken(str: string, type: TokenType): Token {
        let tkn = new Token(str, this.curLine, this.curC - str.length, type);
        return tkn;
    }
    private grabNumericLiteral(): Token {
        let stringValue = "";
        let base = 10;
        if (this.getCurChar() === "0") {
            switch (this.getNextChar()) {
                case "b":
                    base = 2;
                    stringValue = "0b";
                    this.incrementCurIndex(2);
                    break;
                case "o":
                    base = 8;
                    stringValue = "0o";
                    this.incrementCurIndex(2);
                    break;
                case "x":
                    base = 16;
                    stringValue = "0x";
                    this.incrementCurIndex(2);
                    break;
            }
        }
        let number = 0;
        let ch = this.getCurChar();
        let point = false;
        let divideBy = 1;
        while (Lexer.isNumber(ch) || ch === "." || (base == 16 && Lexer.isHexLetter(ch))) {
            stringValue += ch;
            if (ch !== "_") {
                if (ch === ".") {
                    if (point === false) {
                        point = true;
                        this.incrementCurIndex(1);
                        ch = this.getCurChar();
                        continue;
                    } else {
                        this.err.atPoint_PANIC(
                            'Number has too many decimal points "' + number / divideBy + '."',
                            this.curLine,
                            this.curC
                        );
                    }
                }
                let val = Lexer.toInt(ch);
                if (val < base) {
                    number = base * number + val;
                    if (point) divideBy = divideBy * base;
                } else {
                    this.err.atPoint_PANIC(ch + " is an invalid digit symbol in base " + base, this.curLine, this.curC);
                }
            }
            this.incrementCurIndex(1);
            ch = this.getCurChar();
        }

        let value = number / divideBy;
        let tkn = this.makeToken(stringValue, TokenType.NUMERIC_LITERAL_TKN);
        tkn.value = value;
        return tkn;
    }
    private grabStringLiteral(): Token {
        this.grabToken('"', TokenType.DOUBLE_QUOTE_TKN);
        let str = "";
        let ch = this.getCurChar();
        let escapeChar = false;
        while (this.curIndex < this.sourceCode.length && ch !== "\n" && (escapeChar || ch !== '"')) {
            if (!escapeChar || ch === '"') {
                str += ch;
            }
            if (ch === "\\") {
                escapeChar = true;
            } else {
                escapeChar = false;
            }
            this.incrementCurIndex(1);
            ch = this.getCurChar();
        }
        if (this.curIndex >= this.sourceCode.length || ch === "\n") {
            this.err.atPoint_PANIC("Unterminated string literal", this.curLine, this.curC - str.length - 1);
        }
        this.grabToken('"', TokenType.DOUBLE_QUOTE_TKN);
        let tkn = this.makeToken('"' + str + '"', TokenType.STRING_LITERAL_TKN);
        tkn.value = str;
        return tkn;
    }
    private grabDirective(): Token {
        console.assert(this.getCurChar() === "#", "Directives must start with #");
        this.incrementCurIndex(1);
        let str = "#";
        let ch = this.getCurChar();
        while (Lexer.isLetter(ch) || Lexer.isNumber(ch)) {
            this.incrementCurIndex(1);
            str = str + ch;
            ch = this.getCurChar();
        }
        switch (str) {
            case "#range":
                return this.makeToken(str, TokenType.HASH_RANGE_TKN);
            default:
                return this.makeToken(str, TokenType.UNKNOWN_TKN);
        }
    }
    private grabIdentifier(): Token {
        let str = "";
        let ch = this.getCurChar();
        while (Lexer.isLetter(ch) || Lexer.isNumber(ch)) {
            this.incrementCurIndex(1);
            str = str + ch;
            ch = this.getCurChar();
        }
        switch (str) {
            case "type":
                return this.makeToken(str, TokenType.TYPE_TKN);
            case "module":
                return this.makeToken(str, TokenType.MODULE_TKN);
            case "with":
                return this.makeToken(str, TokenType.WITH_TKN);
            case "if":
                return this.makeToken(str, TokenType.IF_TKN);
            case "else":
                return this.makeToken(str, TokenType.ELSE_TKN);
            case "while":
                return this.makeToken(str, TokenType.WHILE_TKN);
            case "for":
                return this.makeToken(str, TokenType.FOR_TKN);
            case "in":
                return this.makeToken(str, TokenType.IN_TKN);
            case "break":
                return this.makeToken(str, TokenType.BREAK_TKN);
            case "continue":
                return this.makeToken(str, TokenType.CONTINUE_TKN);
            case "operator":
                return this.makeToken(str, TokenType.OPERATOR_TKN);

            //Conditionals
            case "true":
                return this.makeToken(str, TokenType.COND_TRUE_TKN);
            case "false":
                return this.makeToken(str, TokenType.COND_FALSE_TKN);

            //Types
            case "mut":
                return this.makeToken(str, TokenType.MUT_CAST_TKN);
            case "const":
                return this.makeToken(str, TokenType.CONST_CAST_TKN);
            case "void":
                return this.makeToken(str, TokenType.VOID_TYPE_TKN);
            case "num":
                return this.makeToken(str, TokenType.NUM_TYPE_TKN);
            case "string":
                return this.makeToken(str, TokenType.STRING_TYPE_TKN);
            case "bool":
                return this.makeToken(str, TokenType.BOOL_TYPE_TKN);

            // Functions
            case "return":
                return this.makeToken(str, TokenType.RETURN_TKN);

            default:
                return this.makeToken(str, TokenType.IDENTIFIER_TKN);
        }
    }
    private consumeSingleLineComment() {
        this.incrementCurIndex(2);
        while (this.curIndex < this.sourceCode.length && this.getCurChar() !== "\n") {
            this.incrementCurIndex(1);
        }
    }
    private consumeBlockComment() {
        let beginLine = this.curLine;
        let beginC = this.curC;
        this.incrementCurIndex(2);
        while (this.curIndex < this.sourceCode.length && !(this.getCurChar() === "*" && this.getNextChar() === "/")) {
            this.incrementCurIndex(1);
        }
        if (this.curIndex >= this.sourceCode.length) {
            this.err
                .atPoint("Beginning of the block comment is as follows", beginLine, beginC)
                .atPoint_PANIC(
                    "Unterminated block comment at the end of the file. Expected */",
                    this.curLine,
                    this.curC
                );
        }
        this.incrementCurIndex(2);
    }
    static isWhitespace(ch: string): boolean {
        return ch === " " || ch === "\t" || ch === "\r" || ch === "\n";
    }
    static isLetter(ch: string): boolean {
        let n = ch.charCodeAt(0);
        return (n >= 65 && n < 91) || (n >= 97 && n < 123) || ch === "'" || ch === "_";
    }
    static isNumber(ch: string): boolean {
        let n = ch.charCodeAt(0);
        return (n >= 48 && n <= 57) || ch === "_";
    }
    static isHexLetter(ch: string): boolean {
        let n = ch.charCodeAt(0);
        return (n >= 65 && n < 71) || (n >= 97 && n < 103);
    }
    static toInt(ch: string): number {
        if (Lexer.isNumber(ch)) return parseInt(ch);
        if (Lexer.isHexLetter(ch)) {
            let val = ch.charCodeAt(0) - 65 + 10;
            if (val <= 15) return val;
            return val - (97 - 65);
        }
        throw "String is not assignable to any int value";
    }
}
