import Token from "./Token";
import TokenType from "./TokenType";

class ErrorHandler {
    private readonly tabWidth = 4;
    private lexer: Lexer;
    private lines: string[] = [];
    constructor(lexer: Lexer) {
        this.lexer = lexer;

        let begin = 0;
        let lineCount = 1;
        for (let i = 0; i < this.lexer.prgmString.length; i++) {
            if (this.lexer.prgmString.charAt(i) === "\n") {
                this.lines[lineCount++] = this.lexer.prgmString.slice(begin, i);
                begin = i + 1;
            }
        }
        this.lines[lineCount] = this.lexer.prgmString.slice(begin, this.lexer.prgmString.length);
    }
    private getColLength(str: string): number {
        let res = 0;
        for (let i = 0; i < str.length; i++) {
            if (str.charAt(i) === "\t") {
                res += this.tabWidth;
            } else {
                res++;
            }
        }
        return res;
    }
    private errorHeader(msg: string, line: number, c?: number): string {
        let optionalCol = "";
        if (c !== undefined) {
            optionalCol = ", col:" + (this.getColLength(this.lines[line].slice(0, c)) + 1);
        }
        return "(line:" + line + optionalCol + "): " + msg + "\n";
    }
    private formatLine(leftIndentSpace: number, line: number, fileLine: string): string {
        if (line < 1) return "";
        let strNumber = line + "";
        for (let i = 0; i < leftIndentSpace - strNumber.length; i++) {
            strNumber = " " + strNumber;
        }
        return "\t" + strNumber + "\t" + fileLine + "\n";
    }
    private getFileLine(line: number): string {
        return this.lines[line].trimLeft().split("\t").join(" ");
    }
    private makeIndicator(line: number, c0: number, c1: number): string {
        console.assert(c0 >= 0, "c0=%s must be non-negative", c0);
        console.assert(c1 > c0, "c1=%s must be larger than c0=%s", c1, c0);
        let indentLen = this.lines[line].length - this.lines[line].trimLeft().length;
        return " ".repeat(c0 - indentLen) + "^".repeat(c1 - c0);
    }
    newErrPoint(msg: string, line: number, c: number): string {
        let error = this.errorHeader(msg, line, c);
        let len = Math.floor(Math.log10(line + 1)) + 1;
        error += this.formatLine(len, line - 1, "");
        error += this.formatLine(len, line, this.getFileLine(line));
        error += this.formatLine(len, line + 1, this.makeIndicator(line, c, c + 1));
        return error;
    }
    newErrToken(msg: string, token: Token): string {
        return this.newErrPoint(msg, token.line, token.c);
    }
    newErrAfterLastToken(msg: string, curToken: Token): string {
        let curLine = curToken.line;
        let curLineStr = this.lines[curLine];
        let cur = curToken.c;
        do {
            if (cur <= 0) {
                curLine--;
                curLineStr = this.lines[curLine];
                cur = curLineStr.length;
            }
            cur--;
        } while (Lexer.isWhitespace(curLineStr.charAt(cur)));

        return this.newErrPoint(msg, curLine, cur + 1);
    }
}
export default class Lexer {
    readonly errHandler: ErrorHandler;
    readonly prgmString: string;
    private curIndex = 0;
    private curLine = 1;
    private curC = 0;
    constructor(prgmString: string) {
        this.prgmString = prgmString;
        this.errHandler = new ErrorHandler(this);
    }
    private err(msg: string, line: number, ch: number) {
        throw this.errHandler.newErrPoint(msg, line, ch);
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
        return this.prgmString.charAt(index);
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
        if (this.curIndex >= this.prgmString.length) {
            return this.grabToken("", TokenType.END_TKN);
        }

        switch (this.getCurChar()) {
            // Assignment
            case "=":
                if (this.getNextChar() === "=") {
                    return this.grabToken("==", TokenType.COND_EQUALS_TKN);
                } else {
                    return this.grabToken("=", TokenType.ASSIGNMENT_TKN);
                }
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
                if (this.getNextChar() === "=") {
                    return this.grabToken("<=", TokenType.COND_LESS_THAN_EQUAL_TKN);
                } else if (this.getNextChar() === "<") {
                    return this.grabToken("<<", TokenType.BIN_SHIFT_LEFT_TKN);
                } else {
                    return this.grabToken("<", TokenType.COND_LESS_THAN_TKN);
                }
            case ":":
                return this.grabToken(":", TokenType.COLON_TKN);

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
                    return this.grabToken("|", TokenType.OP_BAR_TKN);
                }
            case "&":
                if (this.getNextChar() === "&") {
                    return this.grabToken("&&", TokenType.COND_AND_TKN);
                } else {
                    let ampersand = this.grabToken("&" + this.getNextChar(), TokenType.UNKNOWN_TKN);
                    ampersand.value = "&";
                    return ampersand;
                }
            case "$":
                if (this.getNextChar() === "$") {
                    return this.grabToken("$$", TokenType.COND_XOR_TKN);
                } else {
                    let dollar = this.grabToken("$" + this.getNextChar(), TokenType.UNKNOWN_TKN);
                    dollar.value = "$";
                    return this.grabToken("$", TokenType.UNKNOWN_TKN);
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
                        return this.grabToken("->", TokenType.MAPPING_TKN);
                    default:
                        return this.grabToken("-", TokenType.OP_SUBTR_TKN);
                }
            case "*":
                switch (this.getNextChar()) {
                    case "=":
                        return this.grabToken("*=", TokenType.OP_MULT_EQUALS_TKN);
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
                return this.grabToken("~", TokenType.BIN_NOT_TKN);
            case ".":
                if (this.getNextChar() === "." && this.getChar(this.curIndex + 2) === ".") {
                    return this.grabToken("...", TokenType.ELLIPSES_TKN);
                } else {
                    return this.grabToken(".", TokenType.DOT_TKN);
                }
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
        let number = 0;
        let ch = this.getCurChar();
        let point = false;
        let divideBy = 1;
        while (Lexer.isNumber(ch) || ch === ".") {
            stringValue += ch;
            if (ch !== "_") {
                if (ch === ".") {
                    if (point === false) {
                        point = true;
                        this.incrementCurIndex(1);
                        ch = this.getCurChar();
                        continue;
                    } else {
                        this.err(
                            'Number has too many decimal points"' + number / divideBy + '."',
                            this.curLine,
                            this.curC
                        );
                    }
                }
                number = 10 * number + parseInt(ch);
                if (point) divideBy *= 10;
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
        while (escapeChar || ch !== '"') {
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
        this.grabToken('"', TokenType.DOUBLE_QUOTE_TKN);
        let tkn = this.makeToken('"' + str + '"', TokenType.STRING_LITERAL_TKN);
        tkn.value = str;
        return tkn;
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
                return this.makeToken("type", TokenType.TYPE_TKN);
            case "module":
                return this.makeToken("module", TokenType.MODULE_TKN);
            case "with":
                return this.makeToken("with", TokenType.WITH_TKN);
            case "if":
                return this.makeToken("if", TokenType.IF_TKN);
            case "else":
                return this.makeToken("else", TokenType.ELSE_TKN);
            case "while":
                return this.makeToken("while", TokenType.WHILE_TKN);
            case "for":
                return this.makeToken("for", TokenType.FOR_TKN);
            case "in":
                return this.makeToken("in", TokenType.IN_TKN);
            case "break":
                return this.makeToken("break", TokenType.BREAK_TKN);
            case "continue":
                return this.makeToken("continue", TokenType.CONTINUE_TKN);

            //Bitwise Operators
            case "or":
                return this.makeToken("or", TokenType.BIN_OR_TKN);
            case "and":
                return this.makeToken("and", TokenType.BIN_AND_TKN);
            case "xor":
                return this.makeToken("xor", TokenType.BIN_XOR_TKN);

            //Conditionals
            case "true":
                return this.makeToken("true", TokenType.COND_TRUE_TKN);
            case "false":
                return this.makeToken("false", TokenType.COND_FALSE_TKN);

            //Types
            case "void":
                return this.makeToken("void", TokenType.VOID_TKN);
            case "num":
                return this.makeToken("num", TokenType.NUM_TKN);
            case "string":
                return this.makeToken("string", TokenType.STRING_TKN);
            case "bool":
                return this.makeToken("bool", TokenType.BOOL_TKN);

            // lambdas
            case "return":
                return this.makeToken("return", TokenType.RETURN_TKN);

            default:
                return this.makeToken(str, TokenType.IDENTIFIER_TKN);
        }
    }
    private consumeSingleLineComment() {
        this.incrementCurIndex(2);
        while (this.curIndex < this.prgmString.length && this.getCurChar() !== "\n") {
            this.incrementCurIndex(1);
        }
    }
    private consumeBlockComment() {
        this.incrementCurIndex(2);
        while (this.curIndex < this.prgmString.length && !(this.getCurChar() === "*" && this.getNextChar() === "/")) {
            this.incrementCurIndex(1);
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
}
