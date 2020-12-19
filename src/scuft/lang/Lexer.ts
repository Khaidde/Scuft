import Token from "./Token";
import TokenType from "./TokenType";

function isWhitespace(ch: string): boolean {
    return ch === " " || ch === "\t" || ch === "\r" || ch === "\n";
}
function isLetter(ch: string): boolean {
    let n = ch.charCodeAt(0);
    return (n >= 65 && n < 91) || (n >= 97 && n < 123) || ch === "'" || ch === "_";
}
function isNumber(ch: string): boolean {
    let n = ch.charCodeAt(0);
    return (n >= 48 && n <= 57) || ch === "_";
}
function getLine(text: string, line: number): string {
    let lineCount = 1;
    let cur = 0;
    while (lineCount < line) {
        if (text.charAt(cur) === "\n") {
            lineCount++;
        }
        cur++;
    }
    let end = cur;
    while (end < text.length && text.charAt(end) !== "\n") {
        end++;
    }
    return text.substr(cur, end - cur);
}
function strNumber(value: number, length: number): string {
    let str = value + "";
    for (let i = 0; i < length - str.length; i++) {
        str = " " + str;
    }
    return str;
}

function trimLeft(str: string): string {
    let cur = 0;
    let trimmed = "";
    while (isWhitespace(str.charAt(cur))) {
        trimmed += str.charAt(cur);
        cur++;
    }
    return trimmed;
}

class ParseErrorHandler {
    private prgmString: string;
    constructor(prgmString: string) {
        this.prgmString = prgmString;
    }
    errToken(msg: string, token: Token): never {
        let lineStr = getLine(this.prgmString, token.line);
        let cur = 0;
        let ch = lineStr.charAt(0);
        let trimmedAmt = 0;
        while (isWhitespace(ch)) {
            if (ch === "\t") {
                trimmedAmt += 4;
            } else {
                trimmedAmt++;
            }
            cur++;
            ch = lineStr.charAt(cur);
        }
        lineStr = lineStr.slice(cur);
        let indicator = "";
        for (let i = 0; i < token.col - trimmedAmt - 1; i++) {
            indicator += " ";
        }

        let error = "ParseError(line:" + token.line + ", col:" + token.col + "): " + msg + "\n";
        let len = Math.floor(Math.log10(token.line + 1)) + 1;
        if (token.line !== 1) error += strNumber(token.line - 1, len) + "\n";
        error += strNumber(token.line, len) + "\t\t" + lineStr + "\n";
        error += strNumber(token.line + 1, len) + "\t\t" + indicator + "^\n";
        throw error;
    }
}
export default class Lexer {
    readonly parseErr: ParseErrorHandler;
    readonly prgmString: string;
    private curIndex = 0;
    private curLine = 1;
    private curCol = 1;
    constructor(prgmString: string) {
        this.prgmString = prgmString;
        this.parseErr = new ParseErrorHandler(prgmString);
    }
    lex(): Token[] {
        let tokens: Token[] = [];
        let token = this.createToken();
        while (token.type !== TokenType.END_TKN) {
            tokens.push(token);
            token = this.createToken();
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
            this.curCol = 0;
        }
        this.curIndex += amount;
        this.curCol += amount;
    }
    matchAheadTokens(...tokenTypes: TokenType[]): boolean {
        let lastCurIndex = this.curIndex;
        let lastCurCol = this.curCol;
        let lastCurLine = this.curLine;
        let lastlastIndex = this.lastIndex;
        let lastPeekToken = this.peekTkn;

        let res = true;
        for (let i = 0; i < tokenTypes.length; i++) {
            if (tokenTypes[i] !== this.nextToken().type) {
                res = false;
                break;
            }
        }
        this.curIndex = lastCurIndex;
        this.curCol = lastCurCol;
        this.curLine = lastCurLine;
        this.lastIndex = lastlastIndex;
        this.peekTkn = lastPeekToken;
        return res;
    }
    lookAheadToken(amount: number): Token {
        if (amount <= 0) {
            throw "Must look ahead by number greater than 0";
        }
        let lastCurIndex = this.curIndex;
        let lastCurCol = this.curCol;
        let lastCurLine = this.curLine;
        let lastlastIndex = this.lastIndex;
        let lastPeekToken = this.peekTkn;
        for (let i = 0; i < amount - 1; i++) {
            this.nextToken();
        }
        let tkn = this.nextToken();
        this.curIndex = lastCurIndex;
        this.curCol = lastCurCol;
        this.curLine = lastCurLine;
        this.lastIndex = lastlastIndex;
        this.peekTkn = lastPeekToken;
        return tkn;
    }
    private peekTkn!: Token;
    private lastIndex: number = 0;
    peekToken(): Token {
        if (this.curIndex <= this.lastIndex) {
            this.peekTkn = this.createToken();
        }
        return this.peekTkn;
    }
    nextToken(): Token {
        let tkn = this.peekToken();
        this.lastIndex = this.curIndex;
        return tkn;
    }
    private createToken(): Token {
        //Clear Whitespace
        let ch = this.getCurChar();
        while (isWhitespace(ch)) {
            this.incrementCurIndex(1);
            ch = this.getCurChar();
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
                        return this.grabToken("->", TokenType.FUNC_MAPPING_TKN);
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
                        return this.createToken();
                    case "*":
                        this.consumeBlockComment();
                        return this.createToken();
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
                if (isLetter(ch)) {
                    return this.grabIdentifier();
                } else if (isNumber(ch)) {
                    return this.grabNumericLiteral();
                } else {
                    let unknown = this.grabToken(ch, TokenType.UNKNOWN_TKN);
                    unknown.value = ch;
                    return unknown;
                }
        }
    }
    private grabToken(str: string, type: TokenType): Token {
        this.incrementCurIndex(str.length);
        return this.makeToken(str, type);
    }
    private makeToken(str: string, type: TokenType): Token {
        let tkn = new Token(str, this.curLine, this.curCol - str.length, type);
        return tkn;
    }
    private grabNumericLiteral(): Token {
        let stringValue = "";
        let number = 0;
        let ch = this.getCurChar();
        let point = false;
        let divideBy = 1;
        while (isNumber(ch) || ch === ".") {
            stringValue += ch;
            if (ch !== "_") {
                if (ch === ".") {
                    if (point === false) {
                        point = true;
                        this.incrementCurIndex(1);
                        ch = this.getCurChar();
                        continue;
                    } else {
                        throw "";
                        // err(
                        //     'Invalid Number (too many decimal points): "' + number / divideBy + '."',
                        //     this.curLine,
                        //     this.curCol
                        // );
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
        while (isLetter(ch) || isNumber(ch)) {
            this.incrementCurIndex(1);
            str = str + ch;
            ch = this.getCurChar();
        }
        switch (str) {
            case "type":
                return this.makeToken("type", TokenType.TYPE_TKN);
            case "module":
                return this.makeToken("module", TokenType.MODULE_TKN);

            case "for":
                return this.makeToken("for", TokenType.FOR_TKN);
            case "while":
                return this.makeToken("while", TokenType.WHILE_TKN);
            case "break":
                return this.makeToken("break", TokenType.BREAK_TKN);
            case "continue":
                return this.makeToken("continue", TokenType.CONTINUE_TKN);
            case "if":
                return this.makeToken("if", TokenType.IF_TKN);
            case "else":
                return this.makeToken("else", TokenType.ELSE_TKN);

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

            // functions
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
}
