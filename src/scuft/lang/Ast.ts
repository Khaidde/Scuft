import { Token, tokenToStr, TokenType } from "./Token";
import { applyFmt, merge, space, tab, bar } from "./Log";
import Scope from "./Scope";

// export enum ASTFmt {
//     NODE_FMT = "color: #C792DE",
//     TYPE_FMT = "color: #FFEB95",
//     IDENTIFIER_FMT = "color: #348DFF",
//     EXPRESSION_FMT = "color: #E98C6C",
//     FUNCTION_CALL_FMT = "color: #AD2836",
//     WITH_MODULE_FMT = "color: #259F80",
// }

export enum ASTFmt {
    TYPE_FMT = "color: #DE8F6E",
    IDENTIFIER_FMT = "color: #2D93AD",
    EXPRESSION_FMT = "color: #88AB75",

    WITH_MODULE_FMT = "color: #DBD56E",
}

function nodeNameToStr(astNode: ASTNode, fmt: string[]): string {
    return applyFmt(astNode.constructor.name + " (" + astNode.locToken.line + ":" + astNode.locToken.c + ")", fmt);
}

function list(astNodes: ASTNode[], fmt: string[]): string[] {
    let lines: string[] = [];
    for (let i = 0; i < astNodes.length; i++) {
        lines.push(" " + i + ": " + nodeNameToStr(astNodes[i], fmt));

        merge(lines, bar(recurse(astNodes[i], fmt)));
    }
    return lines;
}

export function exprToStr(astExpr: ASTExpression): string {
    switch (astExpr.nodeName) {
        case NodeType.ACCESS_CAST:
            let accessCast = <ASTAccessCast>astExpr;
            return tokenToStr(accessCast.accessType) + " " + exprToStr(accessCast.castedType);
        case NodeType.TYPE_LITERAL:
            return tokenToStr((<ASTTypeLiteral>astExpr).type);
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
        case NodeType.MODULE:
            return "[MODULE_DEF]";
        case NodeType.TYPE_DEF:
            return "[TYPE_DEF]";
        case NodeType.FUNCTION:
            return "[FUNCTION]";
        case NodeType.NAME:
            return (<ASTName>astExpr).refName + "";
        case NodeType.DOT_OP:
            let astDotOp = <ASTDotOperator>astExpr;
            return "(" + exprToStr(astDotOp.rootValue) + "." + exprToStr(astDotOp.memberValue) + ")";
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
        case NodeType.LITERAL:
            return (<ASTLiteral>astExpr).value.stringValue + "";
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
            merge(linesPrgm, ["operatorDefinitions:"]);
            merge(linesPrgm, list(astPrgm.operatorDefinitions, fmt));
            merge(linesPrgm, ["withModules: "]);
            let withModInPrgm = [];
            for (let i = 0; i < astPrgm.withModules.length; i++) {
                withModInPrgm.push(applyFmt(exprToStr(astPrgm.withModules[i]), fmt, ASTFmt.WITH_MODULE_FMT));
            }
            merge(linesPrgm, space(4, withModInPrgm));
            merge(linesPrgm, ["declarations:"]);
            merge(linesPrgm, list(astPrgm.declarations, fmt));
            return linesPrgm;
        case NodeType.BLOCK:
            let linesBlock: string[] = [];
            let astBlock = <ASTBlock>astNode;
            if (astBlock.withModules) {
                merge(linesBlock, ["withModules: "]);
                let withModules = [];
                for (let i = 0; i < astBlock.withModules.length; i++) {
                    withModules.push(applyFmt(exprToStr(astBlock.withModules[i]), fmt, ASTFmt.WITH_MODULE_FMT));
                }
                merge(linesBlock, space(4, withModules));
            }
            merge(linesBlock, ["statements:"]);
            merge(linesBlock, list(astBlock.statements, fmt));
            return linesBlock;
        case NodeType.IF:
            let linesIf: string[] = [];
            let astIf = <ASTIf>astNode;
            merge(linesIf, ["condition: " + applyFmt(exprToStr(astIf.condition), fmt, ASTFmt.EXPRESSION_FMT)]);
            merge(linesIf, ["consequence: " + nodeNameToStr(astIf.consequence, fmt)]);
            merge(linesIf, tab(space(9, recurse(astIf.consequence, fmt))));
            if (astIf.alternative) {
                merge(linesIf, ["alternative: " + nodeNameToStr(astIf.alternative, fmt)]);
                merge(linesIf, tab(space(9, recurse(astIf.alternative, fmt))));
            }
            return linesIf;
        case NodeType.WHILE:
            let linesWhile: string[] = [];
            let astWhile = <ASTWhile>astNode;
            merge(linesWhile, ["condition: " + applyFmt(exprToStr(astWhile.condition), fmt, ASTFmt.EXPRESSION_FMT)]);
            merge(linesWhile, ["block: " + nodeNameToStr(astWhile.block, fmt)]);
            merge(linesWhile, tab(space(3, recurse(astWhile.block, fmt))));
            return linesWhile;
        case NodeType.FOR:
            let linesFor: string[] = [];
            let astFor = <ASTFor>astNode;
            if (astFor.itemParamDec) {
                merge(linesFor, [
                    "itemParamDec:  " + applyFmt(exprToStr(astFor.itemParamDec.lvalue), fmt, ASTFmt.IDENTIFIER_FMT),
                ]);
            }
            if (astFor.indexParamDec) {
                merge(linesFor, [
                    "indexParamDec: " + applyFmt(exprToStr(astFor.indexParamDec.lvalue), fmt, ASTFmt.IDENTIFIER_FMT),
                ]);
            }
            if (astFor.iterableName) {
                merge(linesFor, [
                    "iterableName:  " + applyFmt(exprToStr(astFor.iterableName), fmt, ASTFmt.EXPRESSION_FMT),
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
            merge(linesFor, ["block: " + nodeNameToStr(astFor.block, fmt)]);
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
        case NodeType.OPERATOR_DEF:
            let linesOpDef: string[] = [];
            let astOpDef = <ASTOperatorDefinition>astNode;
            merge(linesOpDef, ["operatorName: " + astOpDef.operatorName.stringValue]);
            merge(linesOpDef, ["functionOverload: " + nodeNameToStr(astOpDef.functionOverload, fmt)]);
            merge(linesOpDef, tab(space(14, recurse(astOpDef.functionOverload, fmt))));
            return linesOpDef;
        case NodeType.DECLARATION:
            let linesDec: string[] = [];
            let astDec = <ASTDeclaration>astNode;
            if (astDec.isAssignment) {
                merge(linesDec, ["lvalue: " + applyFmt(exprToStr(astDec.lvalue), fmt, ASTFmt.EXPRESSION_FMT)]);
            } else {
                merge(linesDec, ["lvalue: " + applyFmt(exprToStr(astDec.lvalue), fmt, ASTFmt.IDENTIFIER_FMT)]);
            }
            if (astDec.type) {
                merge(linesDec, ["type:   " + applyFmt(exprToStr(astDec.type), fmt, ASTFmt.TYPE_FMT)]);
            }
            if (astDec.rvalue) {
                if (
                    astDec.rvalue.nodeName === NodeType.MODULE ||
                    astDec.rvalue.nodeName === NodeType.TYPE_DEF ||
                    astDec.rvalue.nodeName === NodeType.FUNCTION
                ) {
                    merge(linesDec, ["rvalue: " + nodeNameToStr(astDec.rvalue, fmt)]);
                    merge(linesDec, tab(space(4, recurse(astDec.rvalue, fmt))));
                } else {
                    merge(linesDec, ["rvalue: " + applyFmt(exprToStr(astDec.rvalue), fmt, ASTFmt.EXPRESSION_FMT)]);
                }
            }
            return linesDec;
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
                merge(linesFunc, ["returnType: " + applyFmt(exprToStr(astFunc.returnType), fmt, ASTFmt.TYPE_FMT)]);
            }
            merge(linesFunc, ["block: " + nodeNameToStr(astFunc.block, fmt)]);
            merge(linesFunc, tab(space(3, recurse(astFunc.block, fmt))));
            return linesFunc;
        case NodeType.ACCESS_CAST:
        case NodeType.TYPE_LITERAL:
        case NodeType.FUNCTION_TYPE:
        case NodeType.NAME:
        case NodeType.DOT_OP:
        case NodeType.CALL:
        case NodeType.TYPE_CONSTRUCT:
        case NodeType.LITERAL:
        case NodeType.UNARY_OP:
        case NodeType.BINARY_OP:
        case NodeType.LAMBDA:
            return [applyFmt(exprToStr(<ASTExpression>astNode), fmt, ASTFmt.EXPRESSION_FMT)];
        default:
            throw "Can't recursively print tree because given node is not an AST node: " + astNode.constructor.name;
    }
}

export function printAST(astNode: ASTNode) {
    let fmt: string[] = [];
    let outA: string[] = [];
    merge(outA, [nodeNameToStr(astNode, fmt)]);
    merge(outA, recurse(astNode, fmt));

    let str: string[] = [outA.join("\n")];
    for (let i = 0; i < fmt.length; i++) {
        str.push(fmt[i]);
    }
    console.log.apply(printAST, str);
}

export function newLiteralNode(value: number | string | boolean, locToken: Token): ASTLiteral {
    let tkn: Token;
    switch (typeof value) {
        case "number":
            tkn = new Token(value + "", locToken.line, locToken.c, TokenType.NUMERIC_LITERAL_TKN);
            tkn.value = value;
            break;
        case "string":
            tkn = new Token(value + "", locToken.line, locToken.c, TokenType.STRING_LITERAL_TKN);
            tkn.value = value;
            break;
        case "boolean":
            if (value) {
                tkn = new Token(value + "", locToken.line, locToken.c, TokenType.COND_TRUE_TKN);
            } else {
                tkn = new Token(value + "", locToken.line, locToken.c, TokenType.COND_FALSE_TKN);
            }
        default:
            throw "Can't make literal node for value=" + value;
    }
    let literal = new ASTLiteral(tkn);
    literal.value = tkn;
    return literal;
}

export function newTypeLiteralNode(type: TypeLiteralToken, locToken: Token): ASTTypeLiteral {
    let tkn: Token;
    switch (type) {
        case TokenType.MODULE_TKN:
            tkn = new Token("module", locToken.line, locToken.c, TokenType.MODULE_TKN);
            break;
        case TokenType.TYPE_TKN:
            tkn = new Token("type", locToken.line, locToken.c, TokenType.TYPE_TKN);
            break;
        case TokenType.VOID_TYPE_TKN:
            tkn = new Token("void", locToken.line, locToken.c, TokenType.VOID_TYPE_TKN);
            break;
        case TokenType.NUM_TYPE_TKN:
            tkn = new Token("num", locToken.line, locToken.c, TokenType.NUM_TYPE_TKN);
            break;
        case TokenType.STRING_TYPE_TKN:
            tkn = new Token("string", locToken.line, locToken.c, TokenType.STRING_TYPE_TKN);
            break;
        case TokenType.BOOL_TYPE_TKN:
            tkn = new Token("bool", locToken.line, locToken.c, TokenType.BOOL_TYPE_TKN);
            break;
        default:
            throw "Casn't make type literal node for typeID=" + type;
    }
    let typeLiteral = new ASTTypeLiteral(tkn);
    typeLiteral.type = type;
    return typeLiteral;
}

export enum NodeType {
    PROGRAM,
    BLOCK,

    IF,
    WHILE,
    FOR,
    BREAK,
    CONTINUE,
    RETURN,

    OPERATOR_DEF,
    DECLARATION,

    ACCESS_CAST,
    TYPE_LITERAL,
    FUNCTION_TYPE,

    MODULE,
    TYPE_DEF,
    FUNCTION,

    NAME,
    DOT_OP,
    CALL,
    TYPE_CONSTRUCT,

    LITERAL,
    UNARY_OP,
    BINARY_OP,
    LAMBDA,
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

    operatorDefinitions!: ASTOperatorDefinition[];

    withModules!: ASTExpression[];
    declarations!: ASTDeclaration[];
}

export class ASTBlock extends ASTNode {
    nodeName = NodeType.BLOCK;

    scope!: Scope; // The scope of the block

    withModules!: ASTExpression[];
    statements!: ASTStatement[];
}

//Statements
export type ASTStatement =
    | ASTIf
    | ASTWhile
    | ASTFor
    | ASTBreak
    | ASTContinue
    | ASTReturn
    | ASTOperatorDefinition
    | ASTDeclaration
    | ASTExpression;

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

    itemParamDec!: ASTDeclaration;
    indexParamDec!: ASTDeclaration;

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

export class ASTOperatorDefinition extends ASTNode {
    nodeName = NodeType.OPERATOR_DEF;

    operatorName!: Token;
    functionOverload!: ASTFunction;
}

export type DeclarationAccessToken =
    | TokenType.MUTABLE_ASSIGNMENT_TKN
    | TokenType.IMMUTABLE_ASSIGNMENT_TKN
    | TokenType.CONST_ASSIGNMENT_TKN;

export class ASTDeclaration extends ASTNode {
    nodeName = NodeType.DECLARATION;

    inScope!: Scope; // Scope that the declaration is in

    lvalue!: ASTExpression; //Reference to the variable being changed
    type!: ASTExpression;

    resolvedType!: ASTStrictType; // For example, a type of "\(getNumType()) -> string" would have a resolve of "\(num) -> string"

    isAssignment = false;
    accessAssignment!: DeclarationAccessToken; //All variables are immutable by default
    rvalue!: ASTExpression;
}

//Expressions
export type ASTExpression =
    | ASTAccessCast
    | ASTTypeLiteral
    | ASTFunctionType
    | ASTModule
    | ASTTypeDefinition
    | ASTFunction
    | ASTName
    | ASTDotOperator
    | ASTCall
    | ASTTypeConstruction
    | ASTLiteral
    | ASTUnaryOperator
    | ASTBinaryOperator
    | ASTLambda;

export type ASTStrictType = ASTAccessCast | ASTTypeLiteral | ASTFunctionType | ASTName | ASTDotOperator;

export type AccessCastToken = TokenType.MUT_CAST_TKN | TokenType.CONST_CAST_TKN;

export class ASTAccessCast extends ASTNode {
    nodeName = NodeType.ACCESS_CAST;

    accessType!: AccessCastToken;
    castedType!: ASTExpression;
}

export type TypeLiteralToken =
    | TokenType.MODULE_TKN
    | TokenType.TYPE_TKN
    | TokenType.NUM_TYPE_TKN
    | TokenType.STRING_TYPE_TKN
    | TokenType.BOOL_TYPE_TKN
    | TokenType.VOID_TYPE_TKN;

export class ASTTypeLiteral extends ASTNode {
    nodeName = NodeType.TYPE_LITERAL;

    //num, string, bool, void
    type!: TypeLiteralToken;
}

export class ASTFunctionType extends ASTNode {
    nodeName = NodeType.FUNCTION_TYPE;

    inputType!: ASTExpression[]; // parameter types
    outType!: ASTExpression; // num, string, boolean, vector, Particle, Astrology.Star etc...
}

export class ASTModule extends ASTNode {
    nodeName = NodeType.MODULE;

    withModules!: ASTExpression[];
    declarations!: ASTDeclaration[];
}

export class ASTTypeDefinition extends ASTNode {
    nodeName = NodeType.TYPE_DEF;

    withModules!: ASTExpression[];
    declarations!: ASTDeclaration[];
}

export class ASTFunction extends ASTNode {
    nodeName = NodeType.FUNCTION;

    paramDeclaration!: ASTDeclaration[];
    returnType!: ASTExpression;
    block!: ASTBlock;
}

export class ASTName extends ASTNode {
    nodeName = NodeType.NAME;

    refName!: string;
}

export class ASTDotOperator extends ASTNode {
    nodeName = NodeType.DOT_OP;

    rootValue!: ASTExpression;
    memberValue!: ASTExpression;
}

export class ASTCall extends ASTNode {
    nodeName = NodeType.CALL;

    functionNameRef!: ASTExpression; //Function name identifier
    givenParams!: ASTExpression[];
}

export class ASTTypeConstruction extends ASTNode {
    nodeName = NodeType.TYPE_CONSTRUCT;

    typeRef!: ASTExpression;
    assignments!: {
        locToken: Token;

        lvalue: ASTName;
        rvalue: ASTExpression;
    }[]; // Constrained to ASTName lvalues, guaranteed to be unique assignments
}

export class ASTLiteral extends ASTNode {
    nodeName = NodeType.LITERAL;

    // 67, "this is a literal", true
    value!: Token;
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

export class ASTLambda extends ASTNode {
    nodeName = NodeType.LAMBDA;

    parameters!: ASTName[];
    expression!: ASTExpression;
}
