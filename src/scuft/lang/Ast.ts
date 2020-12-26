import Token from "./Token";

enum ASTFmt {
    DEFAULT_FMT = "color: #777;", //"color: #CCDDD5;",
    NODE_FMT = "color: #C792DE",
    TYPE_FMT = "color: #FFEB95",
    IDENTIFIER_FMT = "color: #348DFF",
    EXPRESSION_FMT = "color: #E98C6C",
    FUNCTION_CALL_FMT = "color: #AD2836",
    WITH_MODULE_FMT = "color: #259F80",
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
    const prefix = " ".repeat(amount);
    for (let i = 0; i < strA.length; i++) {
        strA[i] = prefix + strA[i];
    }
    return strA;
}

function __tab(strA: string[]): string[] {
    for (let i = 0; i < strA.length; i++) {
        strA[i] = "    " + strA[i];
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

function __memberStr(astMember: ASTMember): string {
    let memberStr = astMember.rootName.stringValue;
    if (astMember.memberSelect) memberStr += "." + __memberStr(astMember.memberSelect);
    return memberStr;
}

function __exprStr(astExpr: ASTExpression): string {
    switch (astExpr.constructor.name) {
        case ASTLiteral.name:
            return (<ASTLiteral>astExpr).value.stringValue + "";
        case ASTMember.name:
            return __memberStr(<ASTMember>astExpr);
        case ASTTypeConstruction.name:
            let astConstruction = <ASTTypeConstruction>astExpr;
            let constructVal = "";
            for (let i = 0; i < astConstruction.assignments.length; i++) {
                constructVal +=
                    __memberStr(astConstruction.assignments[i].lvalue) +
                    "=" +
                    __exprStr(astConstruction.assignments[i].rvalue);
                if (i + 1 < astConstruction.assignments.length) {
                    constructVal += ", ";
                }
            }
            return __memberStr(astConstruction.typeName) + "{" + constructVal + "}";
        case ASTCall.name:
            let astCall = <ASTCall>astExpr;
            let callVal = __memberStr(astCall.functionName) + "(";
            for (let i = 0; i < astCall.givenParams.length; i++) {
                callVal += __exprStr(astCall.givenParams[i]);
                if (i + 1 < astCall.givenParams.length) {
                    callVal += ", ";
                }
            }
            return callVal + ")";
        case ASTFunction.name:
            return "TODO: Function";
        case ASTUnaryOperator.name:
            return (
                (<ASTUnaryOperator>astExpr).operation.stringValue +
                "(" +
                __exprStr((<ASTUnaryOperator>astExpr).value) +
                ")"
            );
        case ASTBinaryOperator.name:
            let astBinOp = <ASTBinaryOperator>astExpr;
            return (
                "(" +
                __exprStr(astBinOp.lvalue) +
                " " +
                astBinOp.operation.stringValue +
                " " +
                __exprStr(astBinOp.rvalue) +
                ")"
            );
        default:
            throw (
                "Can't recursively print expression because given node is not an AST expression: " +
                astExpr.constructor.name
            );
    }
}

function __recurse(astNode: {}, fmt: string[]): string[] {
    switch (astNode.constructor.name) {
        case ASTProgram.name:
            let linesPrgm: string[] = [];
            let astPrgm = <ASTProgram>astNode;
            __merg(linesPrgm, __a("typeDefinitions: "));
            __merg(linesPrgm, __ls(astPrgm.typeDefinitions, fmt));
            __merg(linesPrgm, __a("modules: "));
            __merg(linesPrgm, __ls(astPrgm.modules, fmt));
            __merg(linesPrgm, __a("statements:"));
            __merg(linesPrgm, __ls(astPrgm.statements, fmt));
            return linesPrgm;
        case ASTModule.name:
            let linesMod: string[] = [];
            let astMod = <ASTModule>astNode;
            __merg(linesMod, __a("name: " + astMod.name.stringValue));
            __merg(linesMod, __a("typeDefinitions: "));
            __merg(linesMod, __ls(astMod.typeDefinitions, fmt));
            __merg(linesMod, __a("statements: "));
            __merg(linesMod, __ls(astMod.statements, fmt));
            return linesMod;
        case ASTTypeDefinition.name:
            let linesTypeDef: string[] = [];
            let astTypeDef = <ASTTypeDefinition>astNode;
            __merg(linesTypeDef, __a("name: " + astTypeDef.name.stringValue));
            __merg(linesTypeDef, __a("typeDeclarations:"));
            __merg(linesTypeDef, __ls(astTypeDef.typeDeclarations, fmt));
            return linesTypeDef;
        case ASTBlock.name:
            let linesBlock: string[] = [];
            let astBlock = <ASTBlock>astNode;
            __merg(linesBlock, __a("withModules: "));
            let withModules = [];
            for (let i = 0; i < astBlock.withModules.length; i++) {
                withModules.push(__fmt(astBlock.withModules[i].stringValue, fmt, ASTFmt.WITH_MODULE_FMT));
            }
            __merg(linesBlock, __s(4, withModules));
            __merg(linesBlock, __a("statements:"));
            __merg(linesBlock, __ls(astBlock.statements, fmt));
            return linesBlock;
        case ASTDeclaration.name:
            let linesDec: string[] = [];
            let astDec = <ASTDeclaration>astNode;
            __merg(linesDec, ["lvalue: " + __fmt(__memberStr(astDec.lvalue), fmt, ASTFmt.IDENTIFIER_FMT)]);
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
                if (astType.type.constructor.name === ASTMember.name) {
                    return __fmtA(__memberStr(<ASTMember>astType.type), fmt, ASTFmt.TYPE_FMT);
                } else {
                    return __fmtA((<Token>astType.type).stringValue, fmt, ASTFmt.TYPE_FMT);
                }
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
        case ASTIf.name:
            let linesIf: string[] = [];
            let astIf = <ASTIf>astNode;
            __merg(linesIf, __a("condition: " + __fmt(__exprStr(astIf.condition), fmt, ASTFmt.EXPRESSION_FMT)));
            __merg(linesIf, __a("consequence: " + __fmt(astIf.consequence.constructor.name, fmt, ASTFmt.NODE_FMT)));
            __merg(linesIf, __tab(__s(9, __recurse(astIf.consequence, fmt))));
            __merg(linesIf, __a("alternative: " + __fmt(astIf.alternative.constructor.name, fmt, ASTFmt.NODE_FMT)));
            __merg(linesIf, __tab(__s(9, __recurse(astIf.alternative, fmt))));
            return linesIf;
        case ASTWhile.name:
            let linesWhile: string[] = [];
            let astWhile = <ASTWhile>astNode;
            __merg(linesWhile, __a("condition: " + __fmt(__exprStr(astWhile.condition), fmt, ASTFmt.EXPRESSION_FMT)));
            __merg(linesWhile, __a("block: " + __fmt(astWhile.block.constructor.name, fmt, ASTFmt.NODE_FMT)));
            __merg(linesWhile, __tab(__s(3, __recurse(astWhile.block, fmt))));
            return linesWhile;
        case ASTFor.name:
            let linesFor: string[] = [];
            let astFor = <ASTFor>astNode;
            if (astFor.itemParamDec) {
                __merg(
                    linesFor,
                    __a("itemParamDec:  " + __fmt(astFor.itemParamDec.stringValue, fmt, ASTFmt.IDENTIFIER_FMT))
                );
            }
            if (astFor.indexParamDec) {
                __merg(
                    linesFor,
                    __a("indexParamDec: " + __fmt(astFor.indexParamDec.stringValue, fmt, ASTFmt.IDENTIFIER_FMT))
                );
            }
            if (astFor.iterableName) {
                __merg(
                    linesFor,
                    __a("iterableName:  " + __fmt(__memberStr(astFor.iterableName), fmt, ASTFmt.IDENTIFIER_FMT))
                );
            }
            if (astFor.lowerBound !== undefined) {
                __merg(
                    linesFor,
                    __a("bounds: " + __fmt(astFor.lowerBound + "..." + astFor.upperBound, fmt, ASTFmt.EXPRESSION_FMT))
                );
            }
            __merg(linesFor, __a("block: " + __fmt(astFor.block.constructor.name, fmt, ASTFmt.NODE_FMT)));
            __merg(linesFor, __tab(__s(3, __recurse(astFor.block, fmt))));
            return linesFor;
        case ASTBreak.name:
        case ASTContinue.name:
            return [];
        case ASTReturn.name:
            let astRet = <ASTReturn>astNode;
            if (astRet.returnValue) {
                return ["returnValue: " + __fmt(__exprStr(astRet.returnValue), fmt, ASTFmt.EXPRESSION_FMT)];
            }
            return [];
        case ASTFunction.name:
            let linesFunc: string[] = [];
            let astFunc = <ASTFunction>astNode;
            __merg(linesFunc, __a("paramDeclaration:"));
            if (astFunc.paramDeclaration) {
                __merg(linesFunc, __ls(astFunc.paramDeclaration, fmt));
            }
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
                __a("functionName: " + __fmt(__memberStr(astCall.functionName), fmt, ASTFmt.FUNCTION_CALL_FMT))
            );
            __merg(linesCall, __a("givenParams:"));
            __merg(linesCall, __ls(astCall.givenParams, fmt));
            return linesCall;
        case ASTLiteral.name:
        case ASTMember.name:
        case ASTTypeConstruction.name:
        case ASTUnaryOperator.name:
        case ASTBinaryOperator.name:
            return ["value: " + __fmt(__exprStr(<ASTExpression>astNode), fmt, ASTFmt.EXPRESSION_FMT)];
        default:
            throw "Can't recursively print tree because given node is not an AST node: " + astNode.constructor.name;
    }
}

export function printAST(astNode: {}) {
    let fmt: string[] = [];
    let outA: string[] = [];
    __merg(outA, __fmtA(astNode.constructor.name, fmt, ASTFmt.NODE_FMT));
    __merg(outA, __recurse(astNode, fmt));

    let str: string[] = [outA.join("\n")];
    for (let i = 0; i < fmt.length; i++) {
        str.push(fmt[i]);
    }
    console.log.apply(printAST, str);
}

export class ASTProgram {
    typeDefinitions!: ASTTypeDefinition[];
    modules!: ASTModule[];
    statements!: ASTStatement[];
}

export class ASTModule {
    name!: Token;
    typeDefinitions!: ASTTypeDefinition[];
    statements!: ASTStatement[];
}

export class ASTTypeDefinition {
    name!: Token;
    typeDeclarations!: ASTDeclaration[];
}

export class ASTBlock {
    withModules!: Token[];
    statements!: ASTStatement[];
}

//Statements
export type ASTStatement = ASTDeclaration | ASTCall | ASTIf | ASTWhile | ASTFor | ASTBreak | ASTContinue | ASTReturn;

export class ASTDeclaration {
    lvalue!: ASTMember;

    // optional (but at least 1 needed)
    type!: ASTType; //Identifier or Type
    rvalue!: ASTExpression | ASTFunction; // expression or a function
}

export class ASTType {
    inputType!: ASTType[]; // "input"
    outType!: ASTType; // "output"

    type!: ASTMember | Token; // num, string, boolean, vector, Particle, Astrology.Star etc...
}

export class ASTIf {
    condition!: ASTExpression;
    consequence!: ASTBlock;
    alternative!: ASTBlock | ASTIf; //Chain if statements together to get "else if" statements
}

export class ASTWhile {
    condition!: ASTExpression;
    block!: ASTBlock;
}

export class ASTFor {
    itemParamDec!: Token;
    indexParamDec!: Token;

    //In statement
    iterableName!: ASTMember;
    lowerBound!: number; //Inclusive
    upperBound!: number; //Exclusive

    block!: ASTBlock;
}

export class ASTBreak {}
export class ASTContinue {}

export class ASTReturn {
    returnValue!: ASTExpression;
}

//Expressions
export type ASTExpression =
    | ASTFunction
    | ASTCall
    | ASTLiteral
    | ASTMember
    | ASTTypeConstruction
    | ASTUnaryOperator
    | ASTBinaryOperator;

export class ASTFunction {
    paramDeclaration!: ASTDeclaration[];
    returnType!: ASTType;
    block!: ASTBlock;
}

export class ASTCall {
    functionName!: ASTMember; //Function name identifier
    givenParams!: ASTExpression[];
}

export class ASTLiteral {
    // 67 or "this is a literal"
    value!: Token;
}

export class ASTMember {
    rootName!: Token;
    memberSelect!: ASTMember;
}

export class ASTTypeConstruction {
    typeName!: ASTMember;
    assignments!: ASTDeclaration[];
}

export class ASTUnaryOperator {
    //a = ~(3 + 4)
    value!: ASTExpression;
    operation!: Token;
}

export class ASTBinaryOperator {
    //3 + 4
    lvalue!: ASTExpression;
    rvalue!: ASTExpression;
    operation!: Token;
}
