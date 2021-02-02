import { Token, tokenToStr, TokenType } from "./Token";
import { applyFmt, merge, space, tab, bar } from "./Log";
import { Scope, ScopeType } from "./Scope";

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
            let outFuncType = "(";
            for (let i = 0; i < astType.inputType.length; i++) {
                outFuncType += exprToStr(astType.inputType[i]);
                if (i + 1 < astType.inputType.length) {
                    outFuncType += ", ";
                }
            }
            outFuncType += ") -> " + exprToStr(astType.outType);
            return outFuncType;
        case NodeType.MODULE:
            return "module {...}";
        case NodeType.TYPE_DEF:
            return exprToStr((<ASTTypeDefinition>astExpr).typeDecRef.lvalue);
        case NodeType.FUNCTION:
            let astFunc = <ASTFunction>astExpr;
            let outFunc = "((";
            for (let i = 0; i < astFunc.paramDeclaration.length; i++) {
                if (astFunc.paramDeclaration[i].rvalue) {
                    outFunc +=
                        exprToStr(astFunc.paramDeclaration[i].lvalue) +
                        " ~= " +
                        exprToStr(astFunc.paramDeclaration[i].rvalue);
                } else {
                    outFunc +=
                        exprToStr(astFunc.paramDeclaration[i].lvalue) +
                        " : " +
                        exprToStr(astFunc.paramDeclaration[i].type);
                }
                if (i + 1 < astFunc.paramDeclaration.length) {
                    outFunc += ", ";
                }
            }
            outFunc += ")";
            if (astFunc.returnType) outFunc += " -> " + exprToStr(astFunc.returnType);
            return outFunc + "{...})";
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
            return exprToStr(astConstruction.typeRef) + ".{" + constructVal + "}";
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
            merge(linesPrgm, ["declarations:"]);
            merge(linesPrgm, list(astPrgm.declarations, fmt));
            return linesPrgm;
        case NodeType.BLOCK:
            let linesBlock: string[] = [];
            let astBlock = <ASTBlock>astNode;
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
            if (astFor.indexParamDec) {
                merge(linesFor, [
                    "indexParamDec: " + applyFmt(exprToStr(astFor.indexParamDec.lvalue), fmt, ASTFmt.IDENTIFIER_FMT),
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
            return ["break"];
        case NodeType.CONTINUE:
            return ["continue"];
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
            if (astDec.type && (astDec.type.nodeName !== NodeType.TYPE_LITERAL || (<ASTTypeLiteral>astDec.type).type)) {
                merge(linesDec, ["type:   " + applyFmt(exprToStr(astDec.type), fmt, ASTFmt.TYPE_FMT)]);
            }
            if (astDec.rvalue) {
                if (
                    (astDec.rvalue.nodeName === NodeType.TYPE_DEF ||
                        astDec.rvalue.nodeName === NodeType.MODULE ||
                        astDec.rvalue.nodeName === NodeType.FUNCTION) &&
                    !(
                        astDec.rvalue.nodeName === NodeType.TYPE_DEF &&
                        astDec.inScope.getEnclosingScope(ScopeType.TYPE_SCOPE)
                    )
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
            merge(linesMod, ["declarations:"]);
            merge(linesMod, list(astMod.declarations, fmt));
            return linesMod;
        case NodeType.TYPE_DEF:
            let linesTypeDef: string[] = [];
            let astTypeDef = <ASTTypeDefinition>astNode;
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
    OVERLOAD_FUNC_TYPE,

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
}

export abstract class ASTNode {
    locToken: Token;
    endToken!: Token;
    nodeName!: NodeType;
    constructor(locToken: Token) {
        this.locToken = locToken;
    }
}

export class ASTProgram extends ASTNode {
    nodeName = NodeType.PROGRAM;

    scope!: Scope;

    declarations!: ASTDeclaration[];
}

export class ASTBlock extends ASTNode {
    nodeName = NodeType.BLOCK;

    scope!: Scope; // The scope of the block

    statements!: ASTStatement[];

    hasJump = false;
    hasReturn = false;
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

    hasJump = false; // Whether or not the if chain has continues/breaks
    hasReturn = false; // Whether or not the if chain fully returns

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

    indexParamDec!: ASTDeclaration;

    //In statement
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

export class ASTDeclaration extends ASTNode {
    nodeName = NodeType.DECLARATION;

    inScope!: Scope; // Scope that the declaration is in

    hasTypeChecked = false;

    lvalue!: ASTExpression; //Reference to the variable being changed
    type!: ASTExpression;

    isAssignment = false;
    accessAssignment!: Token; //DeclarationAccessToken; //All variables are immutable by default
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
    | ASTBinaryOperator;

export type AccessCastToken = TokenType.MUT_CAST_TKN | TokenType.IMMUT_CAST_TKN | TokenType.CONST_CAST_TKN;

export class ASTAccessCast extends ASTNode {
    nodeName = NodeType.ACCESS_CAST;
    resolved!: ASTExpression;

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
    resolved!: ASTExpression;

    //num, string, bool, void
    type!: TypeLiteralToken;
}

export class ASTFunctionType extends ASTNode {
    nodeName = NodeType.FUNCTION_TYPE;
    resolved!: ASTExpression;

    inputType!: ASTExpression[]; // parameter types
    outType!: ASTExpression; // num, string, boolean, vector, Particle, Astrology.Star etc...
}

// Type created during type checking -> Parser can't create this
export class ASTOverloadedFunctionType extends ASTNode {
    nodeName = NodeType.OVERLOAD_FUNC_TYPE;
    resolved!: ASTExpression;

    accessibleFromScope!: Scope; //Where the set of function overloads can be accessed from
    funcNameRef!: ASTName; //Reference a function to pull overloaded types from
}

export class ASTModule extends ASTNode {
    nodeName = NodeType.MODULE;
    resolved!: ASTExpression;

    moduleDecRef!: ASTDeclaration; //Reference to declaration holding the module

    scope!: Scope;
    declarations!: ASTDeclaration[];
}

export class ASTTypeDefinition extends ASTNode {
    nodeName = NodeType.TYPE_DEF;
    resolved!: ASTExpression;

    typeDecRef!: ASTDeclaration; //Reference to the declaration holding the type definition

    scope!: Scope;
    declarations!: ASTDeclaration[];
}

export class ASTFunction extends ASTNode {
    nodeName = NodeType.FUNCTION;
    resolved!: ASTExpression;

    paramDeclaration!: ASTDeclaration[];
    returnType!: ASTExpression;
    block!: ASTBlock;
}

export class ASTName extends ASTNode {
    nodeName = NodeType.NAME;
    resolved!: ASTExpression;

    refName!: string;
}

export class ASTDotOperator extends ASTNode {
    nodeName = NodeType.DOT_OP;
    resolved!: ASTExpression;

    rootValue!: ASTExpression;
    memberValue!: ASTName;
}

export class ASTCall extends ASTNode {
    nodeName = NodeType.CALL;
    resolved!: ASTExpression;

    functionNameRef!: ASTExpression; //Function name identifier
    givenParams!: ASTExpression[];
}

export class ASTTypeConstruction extends ASTNode {
    nodeName = NodeType.TYPE_CONSTRUCT;
    resolved!: ASTExpression;

    typeRef!: ASTExpression;
    assignments!: {
        locToken: Token;

        lvalue: ASTName;
        rvalue: ASTExpression;
    }[]; // Constrained to ASTName lvalues, guaranteed to be unique assignments
}

export class ASTLiteral extends ASTNode {
    nodeName = NodeType.LITERAL;
    resolved!: ASTExpression;

    // 67, "this is a literal", true
    value!: Token;
}

export class ASTUnaryOperator extends ASTNode {
    nodeName = NodeType.UNARY_OP;
    resolved!: ASTExpression;

    //a = ~(3 + 4)
    operation!: Token;
    value!: ASTExpression;
}

export class ASTBinaryOperator extends ASTNode {
    nodeName = NodeType.BINARY_OP;
    resolved!: ASTExpression;

    //3 + 4
    lvalue!: ASTExpression;
    operation!: Token;
    rvalue!: ASTExpression;
}
