import Token from "./Token";

enum ASTFmt {
    DEFAULT_FMT = "color: #777;", //"color: #CCDDD5;",
    NODE_FMT = "color: #C792DE",
    TYPE_FMT = "color: #FFEB95",
    IDENTIFIER_FMT = "color: #348DFF",
    EXPRESSION_FMT = "color: #E98C6C",
    FUNCTION_CALL_FMT = "color: #AD2836",
}

function __fmt(str: string, fmt: string[], format = ASTFmt.DEFAULT_FMT): string {
    fmt.push(format);
    fmt.push(ASTFmt.DEFAULT_FMT);
    return "%c" + str + "%c";
}

function __fmtA(str: string, fmt: string[], format = ASTFmt.DEFAULT_FMT) {
    return [__fmt(str, fmt, format)];
}

function __a(str: string): string[] {
    return [str];
}

function __merg(firstStr: string[], secondStr: string[]): string[] {
    for (let i = 0; i < secondStr.length; i++) {
        firstStr.push(secondStr[i]);
    }
    return firstStr;
}

function __s(amount: number, strA: string[]): string[] {
    let prefix = "";
    for (let i = 0; i < amount; i++) {
        prefix += " ";
    }
    for (let i = 0; i < strA.length; i++) {
        strA[i] = prefix + strA[i];
    }
    return strA;
}

function __tab(strA: string[]): string[] {
    for (let i = 0; i < strA.length; i++) {
        strA[i] = "\t" + strA[i];
    }
    return strA;
}

function __bar(strA: string[]): string[] {
    for (let i = 0; i < strA.length; i++) {
        strA[i] = " |  " + strA[i];
    }
    return strA;
}

function __ls(astNodes: {}[], fmt: string[]): string[] {
    let lines: string[] = [];
    for (let i = 0; i < astNodes.length; i++) {
        lines.push(" " + i + ": " + __fmt(astNodes[i].constructor.name, fmt, ASTFmt.NODE_FMT));

        __merg(lines, __bar(__recurse(astNodes[i], fmt)));
    }
    return lines;
}

export function printAST(astNode: {}) {
    let fmt: string[] = [];
    let outA: string[] = [];
    __merg(outA, __fmtA("ASTProgram", fmt, ASTFmt.NODE_FMT));
    __merg(outA, __recurse(astNode, fmt));

    let str: string[] = [outA.join("\n")];
    for (let i = 0; i < fmt.length; i++) {
        str.push(fmt[i]);
    }
    console.log.apply(printAST, str);
}

function __recurse(astNode: {}, fmt: string[]): string[] {
    switch (astNode.constructor.name) {
        case ASTProgram.name:
            let linesPrgm: string[] = [];
            __merg(linesPrgm, __a("statements:"));
            __merg(linesPrgm, __ls((<ASTProgram>astNode).statements, fmt));
            return linesPrgm;
        case ASTBlock.name:
            let linesBlock: string[] = [];
            __merg(linesBlock, __a("statements:"));
            __merg(linesBlock, __tab(__ls((<ASTBlock>astNode).statements, fmt)));
            return linesBlock;
        case ASTDeclaration.name:
            let linesDec: string[] = [];
            let astDec = <ASTDeclaration>astNode;
            __merg(linesDec, ["lvalue: " + __fmt(astDec.lvalue.stringValue, fmt, ASTFmt.IDENTIFIER_FMT)]);
            if (astDec.type) {
                __merg(linesDec, ["type:   " + __recurse(astDec.type, fmt)]);
            }
            if (astDec.rvalue) {
                __merg(linesDec, ["rvalue: " + __fmt(astDec.rvalue.constructor.name, fmt, ASTFmt.NODE_FMT)]);
                __merg(linesDec, __tab(__s(4, __recurse(astDec.rvalue, fmt))));
            }
            return linesDec;
        case ASTType.name:
            let astType = <ASTType>astNode;
            if (astType.type) {
                return __fmtA(astType.type.stringValue, fmt, ASTFmt.TYPE_FMT);
            } else {
                let out = "(";
                for (let i = 0; i < astType.inputType.length; i++) {
                    out += __recurse(astType.inputType[i], fmt);
                    if (i + 1 < astType.inputType.length) {
                        out += ", ";
                    }
                }
                out += ") -> " + __recurse(astType.outType, fmt);
                return [out];
            }
        case ASTLiteral.name:
            let astNumLit = <ASTLiteral>astNode;
            return ["value: " + __fmt(astNumLit.value.value + "", fmt, ASTFmt.EXPRESSION_FMT)];
        case ASTFunction.name:
            let linesFunc: string[] = [];
            let astFunc = <ASTFunction>astNode;
            __merg(linesFunc, __a("paramDeclaration:"));
            __merg(linesFunc, __ls(astFunc.paramDeclaration, fmt));
            if (astFunc.returnType) {
                __merg(linesFunc, __a("returnType: " + __recurse(astFunc.returnType, fmt)));
            }
            __merg(linesFunc, __a("block: " + __fmt(astFunc.block.constructor.name, fmt, ASTFmt.NODE_FMT)));
            __merg(linesFunc, __tab(__s(3, __recurse(astFunc.block, fmt))));
            return linesFunc;
        case ASTCall.name:
            let linesCall: string[] = [];
            let astCall = <ASTCall>astNode;
            __merg(
                linesCall,
                __a("functionName: " + __fmt(astCall.functionName.stringValue, fmt, ASTFmt.FUNCTION_CALL_FMT))
            );
            __merg(linesCall, __a("givenParams:"));
            __merg(linesCall, __ls(astCall.givenParams, fmt));
            return linesCall;
        default:
            throw "Can't recursively print tree because given node is not an AST node: " + astNode.constructor.name;
    }
}

export class ASTProgram {
    statements: ASTStatement[] = [];
}

export class ASTTypeDeclaration {
    typeDeclarations!: ASTDeclaration[];
}

export type ASTStatement = ASTDeclaration | ASTCall;

export class ASTBlock {
    statements: ASTStatement[] = [];
}

export class ASTDeclaration {
    lvalue!: Token; //Identifier

    // optional (but at least 1 needed)
    type!: ASTType; //Identifier or Type
    rvalue!: ASTExpression | ASTFunction; // expression or a function
}

export class ASTType {
    inputType!: ASTType[]; // "input" or actual type
    outType!: ASTType; // "output"

    type!: Token; // num, string, boolean, vector, Particle, etc...
}

export type ASTExpression = ASTBinaryOperator | ASTLiteral | ASTCall;

export class ASTBinaryOperator {
    //3 + 4
    lvalue!: ASTExpression;
    rvalue!: ASTExpression;
    operation!: Token;
}

export class ASTLiteral {
    // 67 or "this is a literal"
    value!: Token;
}

export class ASTFunction {
    paramDeclaration!: ASTDeclaration[];
    returnType!: ASTType;

    block!: ASTBlock;
}

export class ASTCall {
    functionName!: Token; //Function name identifier
    givenParams!: ASTExpression[];
}
