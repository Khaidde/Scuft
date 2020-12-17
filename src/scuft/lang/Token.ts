import TokenType from "./TokenType";

export default class Token {
    type: TokenType;
    typeName: string;
    line: number;
    col: number;
    ch: number;

    value!: number | string;

    stringValue: string; // string of token in progra
    constructor(type: TokenType, line: number, col: number, ch: number, str: string) {
        this.type = type;
        this.typeName = TokenType[type];
        this.line = line;
        this.col = col;
        this.ch = ch;
        this.stringValue = str;
    }
    getTypeName(): string {
        return TokenType[this.type];
    }
}
