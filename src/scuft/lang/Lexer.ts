import Token from "./Token"
import TokenType from "./TokenType"

import { err } from "../ErrorHandling"

export default class Lexer {
    private prgmString: string;
    private curIndex: number = -1;
    private curLine: number = 1;
    constructor(prgmString: string) {
        this.prgmString = prgmString;
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
    private makeToken(type: TokenType): Token {
        return new Token(type, this.curLine);
    }
    lex(): Token[] {
        let tokens: Token[] = [];
        let token = this.nextToken();
        while (token.type !== TokenType.END_TKN) {
            tokens.push(token);
            token = this.nextToken();
        }
        return tokens;
    }
    nextToken(): Token {
        this.curIndex++;

        let ch = this.getCurChar();
        while (this.isWhitespace(ch)) {
            this.curIndex++;
            ch = this.getCurChar();
        }
        
        if (this.curIndex >= this.prgmString.length) {
            return new Token(TokenType.END_TKN, this.curLine);
        }
        
        switch (this.getCurChar()) {
            // Assignment
            case '=':
                let condEquals = this.grabToken("==", TokenType.COND_EQUALS_TKN);
                if (condEquals) {
                    return condEquals;
                } else {
                    return this.makeToken(TokenType.ASSIGNMENT_TKN); 
                }
            case '>':
                let condGreater = this.grabToken(">=", TokenType.COND_GREATER_THAN_EQUAL_TKN);
                if (condGreater) {
                    return condGreater;
                } else {
                    let shftRight = this.grabToken(">>", TokenType.BIN_SHIFT_RIGHT_TKN);
                    if (shftRight) {
                        return shftRight;
                    } else if (this.getChar(this.curIndex  + 2) === ">") {
                        return this.makeToken(TokenType.BIN_SHIFT_ARITHMETIC_RIGHT_TKN)
                    } else {
                        return this.makeToken(TokenType.COND_GREATER_THAN_TKN); 
                    }
                }
            case '<':
                let condLess = this.grabToken("<=", TokenType.COND_LESS_THAN_EQUAL_TKN);
                if (condLess) {
                    return condLess;
                } else {
                    if (this.getChar(this.curIndex + 1) === "<") {
                        return this.makeToken(TokenType.BIN_SHIFT_LEFT_TKN);
                    } else {
                        return this.makeToken(TokenType.COND_LESS_THAN_TKN); 
                    }
                }
            case ':':
                return this.makeToken(TokenType.COLON_TKN);

            // Blocks
            case '{':
                return this.makeToken(TokenType.LEFT_CURLY_TKN);
            case '}':
                return this.makeToken(TokenType.RIGHT_CURLY_TKN);
            case '[':
                return this.makeToken(TokenType.LEFT_SQUARE_TKN);
            case ']':
                return this.makeToken(TokenType.RIGHT_SQUARE_TKN);
            case '(':
                return this.makeToken(TokenType.LEFT_PARENS_TKN);
            case ')':
                return this.makeToken(TokenType.RIGHT_PARENS_TKN);

            case '~':
                return this.makeToken(TokenType.BIN_NOT_TKN)
            
            //Dot Operator and Ellipses
            case '.':
                if (this.currentMatches("...")) {
                    return this.makeToken(TokenType.ELLIPSES_TKN);
                } else {
                    return this.makeToken(TokenType.DOT_TKN);
                }
            //Conditionals
            case '|':
                let orToken = this.grabToken("||", TokenType.COND_OR_TKN);
                if (orToken) {
                    return orToken;
                } else {
                    return this.makeToken(TokenType.OP_BAR_TKN);
                }
            case '&':
                let andToken = this.grabToken("&&", TokenType.COND_AND_TKN);
                if (andToken) {
                    return andToken;
                } else {
                    return this.makeToken(TokenType.UNKNOWN_TKN);
                }
            case '$':
                let xorToken = this.grabToken("$$", TokenType.COND_XOR_TKN);
                if (xorToken) {
                    return xorToken;
                } else {
                    return this.makeToken(TokenType.UNKNOWN_TKN);
                }
            case '!':
                let notEq = this.grabToken("!=", TokenType.COND_NOT_EQUALS_TKN);
                if (notEq) {
                    return notEq;
                } else {
                    return this.makeToken(TokenType.COND_NOT_TKN);
                }

            // Operators
            case '^':
                return this.makeToken(TokenType.OP_CARROT_TKN)
            case '+':
                switch (this.getNextChar()) {
                    case '=':
                        this.curIndex++;
                        return this.makeToken(TokenType.OP_ADD_EQUALS_TKN);
                    case '+':
                        this.curIndex++;
                        return this.makeToken(TokenType.OP_ADD_ADD_TKN);
                    default:
                        return this.makeToken(TokenType.OP_ADD_TKN);
                }
            case '-':
                switch (this.getNextChar()) {
                    case '=':
                        this.curIndex++;
                        return this.makeToken(TokenType.OP_SUBTR_EQUALS_TKN);
                    case '-':
                        this.curIndex++;
                        return this.makeToken(TokenType.OP_SUBTR_SUBTR_TKN);
                    case '>':
                        this.curIndex++;
                        return this.makeToken(TokenType.FUNC_MAPPING_TKN);
                    default:
                        return this.makeToken(TokenType.OP_SUBTR_TKN);
                }
            case '*':
                switch (this.getNextChar()) {
                    case '=':
                        this.curIndex++;
                        return this.makeToken(TokenType.OP_MULT_EQUALS_TKN);
                    default:
                        return this.makeToken(TokenType.OP_MULT_TKN);
                }
            case '/':
                switch (this.getNextChar()) {
                    case '/':
                        this.curIndex++;
                        return this.makeToken(TokenType.COMMENT_TKN);
                    case '*':
                        this.curIndex++;
                        return this.makeToken(TokenType.BLOCK_COMMENT_TKN);
                    case '=':
                        this.curIndex++;
                        return this.makeToken(TokenType.OP_DIV_EQUALS_TKN);
                    default:
                        return this.makeToken(TokenType.OP_DIVIDE_TKN);
                }

            //Miscellaneous
            case ',':
                return this.makeToken(TokenType.COMMA_TKN);
            case ';':
                return this.makeToken(TokenType.SEMI_COLON_TKN);
            case '\"':
                let str = this.grabString();
                let tkn = this.makeToken(TokenType.STRING_LITERAL_TKN);
                tkn.value = str;
                return tkn;
            default:
                if (this.isLetter(ch)) {
                    let name = this.grabIdentifier();
                    
                    let keyword = this.grabKeywordToken(name);
                    if (keyword) return keyword;

                    let tkn = this.makeToken(TokenType.IDENTIFIER_TKN);
                    tkn.name = name;
                    return tkn;
                } else if (this.isNumber(ch)) {
                    let number = this.grabNumber();
                    let tkn = this.makeToken(TokenType.NUMERIC_LITERAL_TKN);
                    tkn.value = number;
                    return tkn;
                } else {
                    console.log("not letter or string");
                    return this.makeToken(TokenType.UNKNOWN_TKN);
                }
        }
    }
    private grabToken(str: String, type: TokenType) : Token | undefined{
        if (this.currentMatches(str)) {
            this.curIndex += (str.length - 1);
            return this.makeToken(type);
        } else {
            return undefined;
        }
    }
    private grabKeywordToken(name: string): Token | undefined {
        //Keyword checking
        switch (name) {
            case "type":
                return this.makeToken(TokenType.TYPE_TKN);
            case "module":
                return this.makeToken(TokenType.MODULE_TKN);
                
            case "for":
                return this.makeToken(TokenType.FOR_TKN);
            case "while":
                return this.makeToken(TokenType.WHILE_TKN);
            case "break":
                return this.makeToken(TokenType.BREAK_TKN);
            case "continue":
                return this.makeToken(TokenType.CONTINUE_TKN);
            case "if":
                return this.makeToken(TokenType.IF_TKN);
            case "else":
                return this.makeToken(TokenType.ELSE_TKN);
    
            //Bitwise Operators
            case "or":
                return this.makeToken(TokenType.BIN_OR_TKN);
            case "and":
                return this.makeToken(TokenType.BIN_AND_TKN);
            case "xor":
                return this.makeToken(TokenType.BIN_XOR_TKN);
            
            //Conditionals
            case "true":
                return this.makeToken(TokenType.COND_TRUE_TKN);
            case "false":
                return this.makeToken(TokenType.COND_FALSE_TKN);  

            //Types
            case "void":
                return this.makeToken(TokenType.VOID_TKN);
            case "num":
                return this.makeToken(TokenType.NUM_TKN);
            case "string":
                return this.makeToken(TokenType.STRING_TKN);
            case "bool":
                return this.makeToken(TokenType.BOOL_TKN);
            
            // functions
            case "return":
                return this.makeToken(TokenType.RETURN_TKN);
            
            default:
                return undefined;
        }
    }
    private grabIdentifier(): string {
        let str = "";
        let ch = this.getCurChar();
        while (this.isLetter(ch) || this.isNumber(ch)) {
            this.curIndex++;
            str = str + ch;
            ch = this.getCurChar();
        }
        this.curIndex--;
        return str;
    }
    private grabNumber(): number {
        let number = 0;
        let ch = this.getCurChar();
        let point = false;
        let divideBy = 1;
        while (this.isNumber(ch) || ch === ".") {
            if (ch === ".") {
                if (point === false) {
                    point = true;
                    this.curIndex++;
                    ch = this.getCurChar();
                    continue;
                } else {
                    err("Invalid Number (too many decimal points): \"" + number / divideBy + ".\"", this.curLine);
                }
            }
            number = 10 * number + parseInt(ch);
            this.curIndex++;
            ch = this.getCurChar();
            if (point) divideBy *= 10;
        }
        this.curIndex--;
        return number / divideBy;
    }
    private grabString(): string {
        this.curIndex++;
        let str = "";
        let ch = this.getCurChar();
        let escapeChar = false;
        while (escapeChar || ch !== "\"") {
            if (!escapeChar || ch === "\"") {
                str += ch;
            }
            if (ch === "\\") {
                escapeChar = true;
            } else {
                escapeChar = false;
            }
            this.curIndex++;
            ch = this.getCurChar();
        }
        return str;
    }
    private currentMatches(str: String): boolean {
        let len = str.length - 1;
        let currentStr = this.prgmString.slice(this.curIndex, this.curIndex + len);
        return currentStr == str;
    }
    private isWhitespace(ch: string): boolean {
        if (ch === "\n") {
            this.curLine++;
            return true;
        }
        return ch === " " || ch === "\t";
    }
    private isLetter(ch: string): boolean {
        let n = ch.charCodeAt(0);
        return (n >= 65 && n < 91) || (n >= 97 && n < 123) || ch === "'";
    }
    private isNumber(ch: string): boolean {
        let n = ch.charCodeAt(0);
        return (n >= 48 && n <= 57);
    }
}