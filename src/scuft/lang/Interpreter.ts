import * as AST from "./Ast";
import { ErrorHandler } from "./ErrorHandler";
import { Scope } from "./Scope";
import { TokenType } from "./Token";

type Value = number | string | boolean | AST.ASTFunction | undefined;
type Context = {
    parentCtx: Context | undefined;
    scope: Scope;
    values: Map<string, Value>;
};

function printCtx(ctx: Context) {
    let out = "";
    for (const [key, value] of ctx.values.entries()) {
        out += key + " ::: " + value + "\n";
    }
    console.log(out);
}

export default class Interpreter {
    private readonly err: ErrorHandler;
    constructor(errHandler: ErrorHandler) {
        this.err = ErrorHandler.fromHandler("----Interpreter----\n", errHandler);
    }
    interpProgram(prgm: AST.ASTProgram) {
        let globalCtx = { parentCtx: undefined, scope: prgm.scope, values: new Map<string, Value>(), ret: undefined };
        let mainFunc!: AST.ASTFunction;
        for (let i = 0; i < prgm.declarations.length; i++) {
            let globalDecl = prgm.declarations[i];
            if (
                globalDecl.lvalue.nodeName === AST.NodeType.NAME &&
                (<AST.ASTName>globalDecl.lvalue).refName === "main"
            ) {
                if (globalDecl.rvalue.nodeName === AST.NodeType.FUNCTION) {
                    //TODO check if main function signature matches () -> void
                    mainFunc = <AST.ASTFunction>globalDecl.rvalue;
                } else {
                    this.err.atWholeNode_PANIC(
                        "'main' variable declaration expected to be a function but instead was found to be " +
                            AST.exprToStr(globalDecl.type.resolved),
                        globalDecl
                    );
                }
            }
            this.interpDeclaration(prgm.declarations[i], globalCtx);
        }
        if (!mainFunc) {
            this.err.addNote_PANIC(
                "Main function could not be found in global scope. All programs must have a main function denoted as: main = () {...}"
            );
        }
        console.log(
            this.interpBlock(mainFunc.block, {
                parentCtx: globalCtx,
                scope: mainFunc.block.scope,
                values: new Map<string, Value>(),
            })
        );
        printCtx(globalCtx);
    }
    private interpBlock(block: AST.ASTBlock, ctx: Context): [Value] | undefined {
        for (let i = 0; i < block.statements.length - 1; i++) {
            let stmt = block.statements[i];
            switch (stmt.nodeName) {
                case AST.NodeType.DECLARATION:
                    this.interpDeclaration(<AST.ASTDeclaration>stmt, ctx);
                    break;
                default:
                    throw "Unimplemented interpretation of statement";
            }
        }
        //Assume that a break, continue, or return statement will always be the last statement in a block
        let lastStmt = block.statements[block.statements.length - 1];
        switch (lastStmt.nodeName) {
            case AST.NodeType.BREAK:
            case AST.NodeType.CONTINUE:
                throw "Unimplemented interpretation for break and continue statements";
            case AST.NodeType.RETURN:
                let ret = <AST.ASTReturn>lastStmt;
                if (ret.returnValue) return [this.interpExpression(ret.returnValue, ctx)];
                return [undefined];
        }
        return undefined;
    }
    private interpDeclaration(decl: AST.ASTDeclaration, ctx: Context) {
        if (decl.lvalue.nodeName !== AST.NodeType.NAME) throw "Unimplemented declaration thing";

        if (decl.rvalue) {
            let rvalue = decl.rvalue.resolved ? decl.rvalue.resolved : decl.rvalue;
            ctx.values.set((<AST.ASTName>decl.lvalue).refName, this.interpExpression(rvalue, ctx));
        }
    }
    private interpExpression(expr: AST.ASTExpression, ctx: Context): Value {
        if (expr.resolved) expr = expr.resolved;
        switch (expr.nodeName) {
            case AST.NodeType.FUNCTION:
                return <AST.ASTFunction>expr;
            case AST.NodeType.NAME:
                let name = <AST.ASTName>expr;
                let ctxVal = ctx.values.get(name.refName);
                if (ctxVal) {
                    return ctxVal;
                } else {
                    let decl = ctx.scope.table.get(name.refName);
                    if (
                        ctx.scope.isUnordered() ||
                        (decl && decl.accessAssignment.type === TokenType.CONST_ASSIGNMENT_TKN)
                    ) {
                        if (decl) {
                            this.interpDeclaration(decl, ctx);
                            if (decl.rvalue) {
                                return this.interpExpression(decl.rvalue, ctx);
                            }
                        } else {
                            throw "Impossible... type checking should catch that variable doesn't exist";
                        }
                    }
                    this.err.atWholeNode_PANIC("Value of " + AST.exprToStr(name) + " is undefined here", expr);
                }
            case AST.NodeType.CALL:
                let call = <AST.ASTCall>expr;
                let func = call.functionNameRef.resolved;
                if (func.nodeName !== AST.NodeType.FUNCTION) {
                    throw "Impossible, type checker should assert that call ref should refer to function";
                }
                func = <AST.ASTFunction>func;

                let evaluatedCallParams = new Map<string, Value>();
                for (let i = 0; i < call.givenParams.length; i++) {
                    evaluatedCallParams.set(
                        (<AST.ASTName>func.paramDeclaration[i].lvalue).refName,
                        this.interpExpression(call.givenParams[i], ctx)
                    );
                }
                let callCtx = { parentCtx: undefined, scope: func.block.scope, values: evaluatedCallParams };
                let callRet = this.interpBlock(func.block, callCtx);
                if (!callRet) throw "Call doesn't return a value: type checker should have probably caught this";
                return callRet[0];
            case AST.NodeType.LITERAL:
                return (<AST.ASTLiteral>expr).value.value;
            case AST.NodeType.BINARY_OP:
                return this.interpBinaryOperator(<AST.ASTBinaryOperator>expr, ctx);
            default:
                return undefined;
        }
    }
    private interpBinaryOperator(binOp: AST.ASTBinaryOperator, ctx: Context): Value {
        let lEval = this.interpExpression(binOp.lvalue, ctx);
        let rEval = this.interpExpression(binOp.rvalue, ctx);
        switch (binOp.operation.type) {
            case TokenType.COND_OR_TKN:
            case TokenType.COND_AND_TKN:
            case TokenType.COND_XOR_TKN:
            case TokenType.COND_EQUALS_TKN: // Needs to be expanded for num, string, bool, type, etc...
            case TokenType.COND_NOT_EQUALS_TKN: // Same thing as cond_equals
            case TokenType.COND_LESS_THAN_TKN:
            case TokenType.COND_LESS_THAN_EQUAL_TKN:
            case TokenType.COND_GREATER_THAN_TKN:
            case TokenType.COND_GREATER_THAN_EQUAL_TKN:
                throw "Conditional binoperators are unimplemented for interpreter";
            case TokenType.BIN_OR_TKN:
            case TokenType.BIN_AND_TKN:
            case TokenType.BIN_XOR_TKN:
            case TokenType.BIN_SHIFT_RIGHT_TKN:
            case TokenType.BIN_SHIFT_ARITHMETIC_RIGHT_TKN:
            case TokenType.BIN_SHIFT_LEFT_TKN:
                throw "Bin shift operators are unimplemented for interpreter";
            case TokenType.OP_ADD_TKN:
                return <number>lEval + <number>rEval;
            case TokenType.OP_SUBTR_TKN:
                return <number>lEval - <number>rEval;
            case TokenType.OP_MULT_TKN:
                return <number>lEval * <number>rEval;
            case TokenType.OP_DIVIDE_TKN:
                let rightDivisor = <number>rEval;
                if (rightDivisor === 0) {
                    this.err
                        .atWholeNode("Value of divisor found to be 0", binOp.rvalue)
                        .addNote_PANIC(
                            "Thank goodness we caught that. Wouldn't want a giant hole to open up and consume our users :)"
                        );
                }
                return <number>lEval / <number>rEval;
            case TokenType.OP_CARROT_TKN:
            case TokenType.OP_MODULUS_TKN:
            case TokenType.OP_SUBTR_TKN:
            default:
                throw "Unknown binary operator in interpreter";
        }
    }
}
