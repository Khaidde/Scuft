import TokenType from "./TokenType"

export default class Token {
    type: TokenType;
    typeName: string;
    line: number;
    name!: string;
    value!: number | string;
    constructor(type: TokenType, line: number) {
        this.type = type;
        this.typeName = TokenType[type];
        this.line = line;
    }
    getTypeName(): string {
        return TokenType[this.type];
    }
}