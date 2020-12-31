import { Token } from "./Token";
import { applyFmt, merge, space, tab, bar } from "./Log";

export enum ASTFmt {
    NODE_FMT = "color: #C792DE",
    TYPE_FMT = "color: #FFEB95",
    IDENTIFIER_FMT = "color: #348DFF",
    EXPRESSION_FMT = "color: #E98C6C",
    FUNCTION_CALL_FMT = "color: #AD2836",
    WITH_MODULE_FMT = "color: #259F80",
}

function list(astNodes: ASTNode[], fmt: string[]): string[] {
    let lines: string[] = [];
    for (let i = 0; i < astNodes.length; i++) {
        lines.push(" " + i + ": " + applyFmt(astNodes[i].constructor.name, fmt, ASTFmt.NODE_FMT));

        merge(lines, bar(recurse(astNodes[i], fmt)));
    }
    return lines;
}

export function exprToStr(astExpr: ASTExpression): string {
    switch (astExpr.nodeName) {
        case NodeType.MODULE:
            return "[MODULE_DEF]";
        case NodeType.TYPE_DEF:
            return "[TYPE_DEF]";
        case NodeType.FUNCTION:
            return "[FUNCTION]";
        case NodeType.TYPE_LITERAL:
            return (<ASTTypeLiteral>astExpr).type.stringValue + "";
        case NodeType.FUNCTION_TYPE:
            let astType = <ASTFunctionType>astExpr;
            let out = "(";
            for (let i = 0; i < astType.inputType.length; i++) {
                out += exprToStr(astType.inputType[i]);
                if (i + 1 < astType.inputType.length) {
                    out += ", ";
                }
            }
            out += ") -> " + exprToStr(astType.outType);
            return out;
        case NodeType.LITERAL:
            return (<ASTLiteral>astExpr).value.stringValue + "";
        case NodeType.NAME:
            return (<ASTName>astExpr).refName + "";
        case NodeType.TYPE_CONSTRUCT:
            let astConstruction = <ASTTypeConstruction>astExpr;
            let constructVal = "";
            for (let i = 0; i < astConstruction.assignments.length; i++) {
                constructVal +=
                    exprToStr(astConstruction.assignments[i].lvalue) +
                    " <- " +
                    exprToStr(<ASTExpression>astConstruction.assignments[i].rvalue);
                if (i + 1 < astConstruction.assignments.length) {
                    constructVal += ", ";
                }
            }
            return exprToStr(astConstruction.typeRef) + "{" + constructVal + "}";
        case NodeType.LAMBDA:
            let astLambda = <ASTLambda>astExpr;
            let lambdaVal = "\\";
            for (let i = 0; i < astLambda.parameters.length; i++) {
                lambdaVal += astLambda.parameters[i].refName;
                if (i + 1 < astLambda.parameters.length) {
                    lambdaVal += ",";
                }
            }
            return lambdaVal + "=>" + exprToStr(astLambda.expression);
        case NodeType.CALL:
            let astCall = <ASTCall>astExpr;
            let callVal = exprToStr(astCall.functionNameRef) + "(";
            for (let i = 0; i < astCall.givenParams.length; i++) {
                callVal += exprToStr(astCall.givenParams[i]);
                if (i + 1 < astCall.givenParams.length) {
                    callVal += ", ";
                }
            }
            return callVal + ")";
        case NodeType.UNARY_OP:
            return (
                (<ASTUnaryOperator>astExpr).operation.stringValue +
                "(" +
                exprToStr((<ASTUnaryOperator>astExpr).value) +
                ")"
            );
        case NodeType.BINARY_OP:
            let astBinOp = <ASTBinaryOperator>astExpr;
            return (
                "(" +
                exprToStr(astBinOp.lvalue) +
                " " +
                astBinOp.operation.stringValue +
                " " +
                exprToStr(astBinOp.rvalue) +
                ")"
            );
        case NodeType.DOT_OP:
            let astDotOp = <ASTDotOperator>astExpr;
            return "(" + exprToStr(astDotOp.rootValue) + "." + exprToStr(astDotOp.memberValue) + ")";
        default:
            throw (
                "Can't recursively print expression because given node is not an AST expression: " +
                astExpr.constructor.name
            );
    }
}

export function recurse(astNode: ASTNode, fmt: string[]): string[] {
    switch (astNode.nodeName) {
        case NodeType.PROGRAM:
            let linesPrgm: string[] = [];
            let astPrgm = <ASTProgram>astNode;
            merge(linesPrgm, ["statements:"]);
            merge(linesPrgm, list(astPrgm.statements, fmt));
            return linesPrgm;
        case NodeType.BLOCK:
            let linesBlock: string[] = [];
            let astBlock = <ASTBlock>astNode;
            merge(linesBlock, ["withModules: "]);
            let withModules = [];
            for (let i = 0; i < astBlock.withModules.length; i++) {
                withModules.push(applyFmt(exprToStr(astBlock.withModules[i]), fmt, ASTFmt.WITH_MODULE_FMT));
            }
            merge(linesBlock, space(4, withModules));
            merge(linesBlock, ["statements:"]);
            merge(linesBlock, list(astBlock.statements, fmt));
            return linesBlock;
        case NodeType.DECLARATION:
            let linesDec: string[] = [];
            let astDec = <ASTDeclaration>astNode;
            merge(linesDec, ["lvalue: " + applyFmt(exprToStr(astDec.lvalue), fmt, ASTFmt.IDENTIFIER_FMT)]);
            if (astDec.type) {
                merge(linesDec, ["type:   " + applyFmt(exprToStr(astDec.type), fmt, ASTFmt.TYPE_FMT)]);
            }
            if (astDec.rvalue) {
                merge(linesDec, ["rvalue: " + applyFmt(astDec.rvalue.constructor.name, fmt, ASTFmt.NODE_FMT)]);
                merge(linesDec, tab(space(4, recurse(astDec.rvalue, fmt))));
            }
            return linesDec;
        case NodeType.IF:
            let linesIf: string[] = [];
            let astIf = <ASTIf>astNode;
            merge(linesIf, ["condition: " + applyFmt(exprToStr(astIf.condition), fmt, ASTFmt.EXPRESSION_FMT)]);
            merge(linesIf, ["consequence: " + applyFmt(astIf.consequence.constructor.name, fmt, ASTFmt.NODE_FMT)]);
            merge(linesIf, tab(space(9, recurse(astIf.consequence, fmt))));
            if (astIf.alternative) {
                merge(linesIf, ["alternative: " + applyFmt(astIf.alternative.constructor.name, fmt, ASTFmt.NODE_FMT)]);
                merge(linesIf, tab(space(9, recurse(astIf.alternative, fmt))));
            }
            return linesIf;
        case NodeType.WHILE:
            let linesWhile: string[] = [];
            let astWhile = <ASTWhile>astNode;
            merge(linesWhile, ["condition: " + applyFmt(exprToStr(astWhile.condition), fmt, ASTFmt.EXPRESSION_FMT)]);
            merge(linesWhile, ["block: " + applyFmt(astWhile.block.constructor.name, fmt, ASTFmt.NODE_FMT)]);
            merge(linesWhile, tab(space(3, recurse(astWhile.block, fmt))));
            return linesWhile;
        case NodeType.FOR:
            let linesFor: string[] = [];
            let astFor = <ASTFor>astNode;
            if (astFor.itemParamDec) {
                merge(linesFor, [
                    "itemParamDec:  " + applyFmt(exprToStr(astFor.itemParamDec), fmt, ASTFmt.IDENTIFIER_FMT),
                ]);
            }
            if (astFor.indexParamDec) {
                merge(linesFor, [
                    "indexParamDec: " + applyFmt(exprToStr(astFor.indexParamDec), fmt, ASTFmt.IDENTIFIER_FMT),
                ]);
            }
            if (astFor.iterableName) {
                merge(linesFor, [
                    "iterableName:  " + applyFmt(exprToStr(astFor.iterableName), fmt, ASTFmt.IDENTIFIER_FMT),
                ]);
            }
            if (astFor.lowerBound !== undefined) {
                merge(linesFor, [
                    "bounds: " +
                        applyFmt(
                            exprToStr(astFor.lowerBound) + "..." + exprToStr(astFor.upperBound),
                            fmt,
                            ASTFmt.EXPRESSION_FMT
                        ),
                ]);
            }
            merge(linesFor, ["block: " + applyFmt(astFor.block.constructor.name, fmt, ASTFmt.NODE_FMT)]);
            merge(linesFor, tab(space(3, recurse(astFor.block, fmt))));
            return linesFor;
        case NodeType.BREAK:
        case NodeType.CONTINUE:
            return [];
        case NodeType.RETURN:
            let astRet = <ASTReturn>astNode;
            if (astRet.returnValue) {
                return ["returnValue: " + applyFmt(exprToStr(astRet.returnValue), fmt, ASTFmt.EXPRESSION_FMT)];
            }
            return [];
        case NodeType.MODULE:
            let linesMod: string[] = [];
            let astMod = <ASTModule>astNode;
            merge(linesMod, ["withModules: "]);
            let withModInMod = [];
            for (let i = 0; i < astMod.withModules.length; i++) {
                withModInMod.push(applyFmt(exprToStr(astMod.withModules[i]), fmt, ASTFmt.WITH_MODULE_FMT));
            }
            merge(linesMod, space(4, withModInMod));
            merge(linesMod, ["declarations:"]);
            merge(linesMod, list(astMod.declarations, fmt));
            return linesMod;
        case NodeType.TYPE_DEF:
            let linesTypeDef: string[] = [];
            let astTypeDef = <ASTTypeDefinition>astNode;
            merge(linesTypeDef, ["withModules: "]);
            let withModInType = [];
            for (let i = 0; i < astTypeDef.withModules.length; i++) {
                withModInType.push(applyFmt(exprToStr(astTypeDef.withModules[i]), fmt, ASTFmt.WITH_MODULE_FMT));
            }
            merge(linesTypeDef, space(4, withModInType));
            merge(linesTypeDef, ["declarations:"]);
            merge(linesTypeDef, list(astTypeDef.declarations, fmt));
            return linesTypeDef;
        case NodeType.FUNCTION:
            let linesFunc: string[] = [];
            let astFunc = <ASTFunction>astNode;
            merge(linesFunc, ["paramDeclaration:"]);
            if (astFunc.paramDeclaration) {
                merge(linesFunc, list(astFunc.paramDeclaration, fmt));
            }
            if (astFunc.returnType) {
                merge(linesFunc, ["returnType: " + recurse(astFunc.returnType, fmt)]);
            }
            merge(linesFunc, ["block: " + applyFmt(astFunc.block.constructor.name, fmt, ASTFmt.NODE_FMT)]);
            merge(linesFunc, tab(space(3, recurse(astFunc.block, fmt))));
            return linesFunc;
        case NodeType.TYPE_LITERAL:
        case NodeType.FUNCTION_TYPE:
        case NodeType.LITERAL:
        case NodeType.NAME:
        case NodeType.TYPE_CONSTRUCT:
        case NodeType.LAMBDA:
        case NodeType.CALL:
        case NodeType.UNARY_OP:
        case NodeType.BINARY_OP:
        case NodeType.DOT_OP:
            return [applyFmt(exprToStr(<ASTExpression>astNode), fmt, ASTFmt.EXPRESSION_FMT)];
        default:
            throw "Can't recursively print tree because given node is not an AST node: " + astNode.constructor.name;
    }
}

export function printAST(astNode: ASTNode) {
    let fmt: string[] = [];
    let outA: string[] = [];
    merge(outA, [applyFmt(astNode.constructor.name, fmt, ASTFmt.NODE_FMT)]);
    merge(outA, recurse(astNode, fmt));

    let str: string[] = [outA.join("\n")];
    for (let i = 0; i < fmt.length; i++) {
        str.push(fmt[i]);
    }
    console.log.apply(printAST, str);
}

export function isExpressionEqual(e0: ASTExpression, e1: ASTExpression): boolean {
    if (e0.nodeName !== e1.nodeName) return false;
    switch (e0.nodeName) {
        case NodeType.TYPE_LITERAL:
            let typeLit0 = <ASTTypeLiteral>e0;
            let typeLit1 = <ASTTypeLiteral>e1;
            return typeLit0.type.typeEquals(typeLit1.type);
        case NodeType.FUNCTION_TYPE:
            let func0 = <ASTFunctionType>e0;
            let func1 = <ASTFunctionType>e1;
            if (!isExpressionEqual(func0.outType, func1.outType)) return false;
            for (let i = 0; i < func0.inputType.length; i++) {
                if (!isExpressionEqual(func0.inputType[i], func1.inputType[i])) return false;
            }
            return true;
        case NodeType.NAME:
            let name0 = <ASTName>e0;
            let name1 = <ASTName>e1;
            return name0.refName === name1.refName;
    }
    return false;
}

export enum NodeType {
    PROGRAM,
    BLOCK,

    DECLARATION,
    SINGLE_DECLARATION,
    IF,
    WHILE,
    FOR,
    BREAK,
    CONTINUE,
    RETURN,

    MODULE,
    TYPE_DEF,
    FUNCTION,

    TYPE_LITERAL,
    FUNCTION_TYPE,

    LITERAL,
    NAME,
    TYPE_CONSTRUCT,
    LAMBDA,
    CALL,
    UNARY_OP,
    BINARY_OP,
    DOT_OP,
}

export abstract class ASTNode {
    locToken: Token;
    nodeName!: NodeType;
    constructor(locToken: Token) {
        this.locToken = locToken;
    }
}

export class ASTProgram extends ASTNode {
    nodeName = NodeType.PROGRAM;

    statements!: ASTStatement[];
}

export type ASTReference = ASTName | ASTDotOperator | ASTCall;

export class ASTBlock extends ASTNode {
    nodeName = NodeType.BLOCK;

    withModules!: ASTReference[];
    statements!: ASTStatement[];
}

//Statements
export type ASTStatement =
    | ASTDeclaration
    | ASTIf
    | ASTWhile
    | ASTFor
    | ASTBreak
    | ASTContinue
    | ASTReturn
    | ASTExpression;

export class ASTDeclaration extends ASTNode {
    nodeName = NodeType.DECLARATION;

    lvalue!: ASTReference; //Reference to the variable being changed

    // optional (but at least 1 needed)
    type!: ASTType;
    rvalue!: ASTExpression;
}

//Used in type definition, type constructor, and function parameters
export class ASTSingleVarDeclaration extends ASTDeclaration {
    nodeName = NodeType.SINGLE_DECLARATION;

    lvalue!: ASTName; //Reference to the variable being changed
}

export class ASTIf extends ASTNode {
    nodeName = NodeType.IF;

    condition!: ASTExpression;
    consequence!: ASTBlock;
    alternative!: ASTBlock | ASTIf; //Chain if statements together to get "else if" statements
}

export class ASTWhile extends ASTNode {
    nodeName = NodeType.WHILE;

    condition!: ASTExpression;
    block!: ASTBlock;
}

export class ASTFor extends ASTNode {
    nodeName = NodeType.FOR;

    itemParamDec!: ASTName;
    indexParamDec!: ASTName;

    //In statement
    iterableName!: ASTExpression;
    lowerBound!: ASTExpression;
    upperBound!: ASTExpression;

    block!: ASTBlock;
}

export class ASTBreak extends ASTNode {
    nodeName = NodeType.BREAK;
}
export class ASTContinue extends ASTNode {
    nodeName = NodeType.CONTINUE;
}

export class ASTReturn extends ASTNode {
    nodeName = NodeType.RETURN;

    returnValue!: ASTExpression;
}

//Expressions
export type ASTExpression =
    | ASTModule
    | ASTTypeDefinition
    | ASTFunction
    | ASTTypeLiteral
    | ASTFunctionType
    | ASTLiteral
    | ASTName
    | ASTTypeConstruction
    | ASTLambda
    | ASTCall
    | ASTUnaryOperator
    | ASTBinaryOperator
    | ASTDotOperator;

export class ASTModule extends ASTNode {
    nodeName = NodeType.MODULE;

    withModules!: ASTReference[];
    declarations!: ASTDeclaration[];
}

export class ASTTypeDefinition extends ASTNode {
    nodeName = NodeType.TYPE_DEF;

    withModules!: ASTReference[];
    declarations!: ASTSingleVarDeclaration[];
}

export class ASTFunction extends ASTNode {
    nodeName = NodeType.FUNCTION;

    paramDeclaration!: ASTSingleVarDeclaration[];
    returnType!: ASTExpression;
    block!: ASTBlock;
}

//Types
export type ASTType = ASTReference | ASTTypeLiteral | ASTFunctionType;

export class ASTTypeLiteral extends ASTNode {
    nodeName = NodeType.TYPE_LITERAL;

    //num, string, bool, void
    type!: Token;
}

export class ASTFunctionType extends ASTNode {
    nodeName = NodeType.FUNCTION_TYPE;

    inputType!: ASTExpression[]; // parameter types
    outType!: ASTExpression; // num, string, boolean, vector, Particle, Astrology.Star etc...
}

export class ASTLiteral extends ASTNode {
    nodeName = NodeType.LITERAL;

    // 67, "this is a literal", true
    value!: Token;
}

export class ASTName extends ASTNode {
    nodeName = NodeType.NAME;

    refName!: string;
}

export class ASTTypeConstruction extends ASTNode {
    nodeName = NodeType.TYPE_CONSTRUCT;

    typeRef!: ASTExpression;
    assignments!: ASTSingleVarDeclaration[];
}

export class ASTLambda extends ASTNode {
    nodeName = NodeType.LAMBDA;

    parameters!: ASTName[];
    expression!: ASTExpression;
}

export class ASTCall extends ASTNode {
    nodeName = NodeType.CALL;

    functionNameRef!: ASTExpression; //Function name identifier
    givenParams!: ASTExpression[];
}

export class ASTUnaryOperator extends ASTNode {
    nodeName = NodeType.UNARY_OP;

    //a = ~(3 + 4)
    operation!: Token;
    value!: ASTExpression;
}

export class ASTBinaryOperator extends ASTNode {
    nodeName = NodeType.BINARY_OP;

    //3 + 4
    lvalue!: ASTExpression;
    operation!: Token;
    rvalue!: ASTExpression;
}

export class ASTDotOperator extends ASTNode {
    nodeName = NodeType.DOT_OP;

    rootValue!: ASTExpression;
    memberValue!: ASTExpression;
}
