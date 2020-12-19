import TokenType from "./TokenType";

export default class Token {
    stringValue: string; // string of token in written program

    line: number;
    col: number;

    type: TokenType;
    typeName: string;

    value!: number | string;

    constructor(stringValue: string, line: number, col: number, type: TokenType) {
        this.stringValue = stringValue;

        this.line = line;
        this.col = col;

        this.type = type;
        this.typeName = TokenType[type];
    }
}
