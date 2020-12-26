import TokenType from "./TokenType";

export default class Token {
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
}
