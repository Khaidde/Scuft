import * as AST from "./Ast";
import { ErrorHandler } from "./ErrorHandler";
import { printScope, Scope, ScopeType } from "./Scope";
import { Token, TokenType } from "./Token";

export default class TypeChecker {
    private readonly err: ErrorHandler;
    constructor(errHandler: ErrorHandler) {
        this.err = ErrorHandler.fromHandler("----TypeChecker----\n", errHandler);
    }
    private castAccess(typeExpr: AST.ASTExpression, accessCast: AST.AccessCastToken): AST.ASTExpression {
        if (accessCast === TokenType.IMMUT_CAST_TKN) {
            if (typeExpr.nodeName === AST.NodeType.ACCESS_CAST) {
                return (<AST.ASTAccessCast>typeExpr).castedType;
            } else {
                return typeExpr;
            }
        }
        let cast = new AST.ASTAccessCast(typeExpr.locToken);
        cast.accessType = accessCast;
        cast.castedType = typeExpr;
        return this.resolveAccessCast(cast);
    }
    private checkAccessCast(typeExpr: AST.ASTExpression, accessCast: AST.AccessCastToken): boolean {
        if (typeExpr.nodeName === AST.NodeType.ACCESS_CAST) {
            return accessCast === (<AST.ASTAccessCast>typeExpr).accessType;
        } else {
            return accessCast === TokenType.IMMUT_CAST_TKN;
        }
    }
    // Resolve "const const" into "const" and other similar optimizations
    private resolveAccessCast(accessCast: AST.ASTAccessCast): AST.ASTAccessCast {
        if (accessCast.castedType.nodeName === AST.NodeType.ACCESS_CAST) {
            let isInMut = (<AST.ASTAccessCast>accessCast.castedType).accessType === TokenType.MUT_CAST_TKN;
            if (accessCast.accessType === TokenType.MUT_CAST_TKN) {
                if (isInMut) {
                    // mut mut => mut
                    // TODO console.warn("mut mut, WHAT ARE YOU DOING?!?!?!");
                } else {
                    // mut const
                    this.err.atNode_PANIC("Can't cast const type to mutable", accessCast);
                }
            } else {
                if (isInMut) {
                    // const mut(num)
                    // TODO console.warn("const mut, YOU SIMPLETON!!!!");
                } else {
                    // const const
                    // TODO console.warn("const const, why? What benefit does this provide you?");
                }
            }
            accessCast.castedType = (<AST.ASTAccessCast>accessCast.castedType).castedType; //Optimize the access casts
        }
        return accessCast;
    }
    // Compares two expressions (presumed to be types) and returns whether or not they are equivalent
    // "(num, num) -> num" and "(num, pureGetNumType()) -> num" returns true assuming that pureGetNumType returns "num"
    // Note: "mut num" is NOT equal to "num"
    private isTypeEqual(t0: AST.ASTExpression, t1: AST.ASTExpression, scope: Scope): boolean {
        if (!t0.resolved) this.resolveConstExpression(t0, scope);
        if (!t1.resolved) this.resolveConstExpression(t1, scope);
        t1 = t1.resolved;
        t0 = t0.resolved;
        if (t0.nodeName !== t1.nodeName) return false;
        switch (t0.nodeName) {
            case AST.NodeType.ACCESS_CAST:
                let accessCast0 = <AST.ASTAccessCast>t0;
                let accessCast1 = <AST.ASTAccessCast>t1;
                if (accessCast0.accessType !== accessCast1.accessType) return false;
                return this.isTypeEqual(accessCast0.castedType, accessCast1.castedType, scope);
            case AST.NodeType.TYPE_LITERAL:
                let typeLit0 = <AST.ASTTypeLiteral>t0;
                let typeLit1 = <AST.ASTTypeLiteral>t1;
                return typeLit0.type === typeLit1.type;
            case AST.NodeType.FUNCTION_TYPE:
                let func0 = <AST.ASTFunctionType>t0;
                let func1 = <AST.ASTFunctionType>t1;

                if (!this.isTypeEqual(func0.outType, func1.outType, scope)) return false;
                for (let i = 0; i < func0.inputType.length; i++) {
                    if (!this.isTypeEqual(func0.inputType[i], func1.inputType[i], scope)) return false;
                }
                return true;
            case AST.NodeType.TYPE_DEF:
                let typeDef0 = <AST.ASTTypeConstruction>t0;
                let typeDef1 = <AST.ASTTypeConstruction>t1;
                return typeDef0.locToken.equals(typeDef1.locToken);
            default:
                throw "Expression is not a pure type and can't be used in isTypeEqual";
        }
    }
    // Ignores access casts: "bool" === "bool", "mut num" === "num" but "const string" !== "num"
    private isEqualToUncastedType(typeExpr: AST.ASTExpression, typeLiteral: AST.TypeLiteralToken): boolean {
        if (typeExpr.nodeName === AST.NodeType.ACCESS_CAST) {
            return this.isEqualToUncastedType((<AST.ASTAccessCast>typeExpr).castedType, typeLiteral);
        }
        if (typeExpr.nodeName !== AST.NodeType.TYPE_LITERAL) return false;
        return (<AST.ASTTypeLiteral>typeExpr).type === typeLiteral;
    }
    private resolveTypeExpression(expr: AST.ASTExpression, scope: Scope) {
        this.resolveConstExpression(expr, scope);
        if (!this.isLooseType(expr.resolved)) {
            this.err.atWholeNode_PANIC(
                "Expression expected to be a type but got " + AST.exprToStr(expr.resolved) + " instead",
                expr
            );
        }
    }
    // Determine if expr is roughly a type. Guaranteed to work if expr was const evaluated downwards
    // Use sparingly
    private isLooseType(expr: AST.ASTExpression): boolean {
        switch (expr.nodeName) {
            case AST.NodeType.ACCESS_CAST:
                let accessCast = <AST.ASTAccessCast>expr;
                return this.isLooseType(accessCast.castedType);
            case AST.NodeType.TYPE_LITERAL:
                return true;
            case AST.NodeType.FUNCTION_TYPE:
                let funcType = <AST.ASTFunctionType>expr;
                for (let i = 0; i < funcType.inputType.length; i++) {
                    if (!this.isLooseType(funcType.inputType[i])) return false;
                }
                return this.isLooseType(funcType.outType);
            case AST.NodeType.TYPE_DEF:
                return true;
            default:
                return false;
        }
    }
    private resolveConstExpression(expr: AST.ASTExpression, scope: Scope) {
        this.recurseResConstExpression(expr, expr, scope);
    }
    // TODO check for cycles
    private recurseResConstExpression(expr: AST.ASTExpression, sourceExpr: AST.ASTExpression, scope: Scope) {
        if (expr.resolved) return;
        switch (expr.nodeName) {
            case AST.NodeType.ACCESS_CAST:
                //Note: "mut num" is a constant expression because the type expression itself is compile-time known
                let accessCast = this.resolveAccessCast(<AST.ASTAccessCast>expr);
                this.resolveTypeExpression(accessCast.castedType, scope);
                expr.resolved = accessCast;
                break;
            case AST.NodeType.TYPE_LITERAL:
                expr.resolved = expr;
                break;
            case AST.NodeType.FUNCTION_TYPE:
                let astFuncType = <AST.ASTFunctionType>expr;
                for (let i = 0; i < astFuncType.inputType.length; i++) {
                    this.resolveTypeExpression(astFuncType.inputType[i], scope);
                }
                this.resolveTypeExpression(astFuncType.outType, scope);
                expr.resolved = expr;
                break;
            case AST.NodeType.TYPE_DEF:
            case AST.NodeType.MODULE:
                expr.resolved = expr;
                break;
            case AST.NodeType.FUNCTION:
                let astFunc = <AST.ASTFunction>expr;
                this.assertPureFunction(astFunc, astFunc);
                expr.resolved = expr;
                break;
            case AST.NodeType.NAME:
                let astName = <AST.ASTName>expr;
                // Order of the declaration doesn't matter because nameDecl is assumed to be constant
                let nameDecl = this.searchDeclarationName(astName, scope);
                if (nameDecl) {
                    if (
                        !nameDecl.accessAssignment ||
                        nameDecl.accessAssignment.type !== TokenType.CONST_ASSIGNMENT_TKN
                    ) {
                        this.err
                            .atWholeNode(
                                "Only constant expressions can be used to define other constant expressions",
                                sourceExpr
                            )
                            .atNode_PANIC("This declaration was expected to be constant", nameDecl);
                    }
                    this.recurseResConstExpression(nameDecl.rvalue, sourceExpr, scope); //Assume that nameDecl is a constant declaration so it MUST have a right hand side
                    expr.resolved = nameDecl.rvalue.resolved;
                    break;
                } else {
                    this.err.atNode_PANIC("Couldn't find constant variable '" + AST.exprToStr(astName) + "'", astName);
                }
            case AST.NodeType.DOT_OP:
                let refDot = <AST.ASTDotOperator>expr;
                this.recurseResConstExpression(refDot.rootValue, sourceExpr, scope);
                let rootDec!: AST.ASTDeclaration;
                let accessScope!: Scope;
                if (refDot.rootValue.resolved.nodeName === AST.NodeType.MODULE) {
                    let mod = <AST.ASTModule>refDot.rootValue.resolved;
                    rootDec = mod.moduleDecRef;
                    accessScope = mod.scope;
                } else if (refDot.rootValue.resolved.nodeName === AST.NodeType.TYPE_DEF) {
                    let typeDef = <AST.ASTTypeDefinition>refDot.rootValue.resolved;
                    rootDec = typeDef.typeDecRef;
                    accessScope = typeDef.scope;
                } else if (refDot.rootValue.resolved.nodeName === AST.NodeType.TYPE_CONSTRUCT) {
                    let typeConstruct = <AST.ASTTypeConstruction>refDot.rootValue.resolved;
                    let typeDef = <AST.ASTTypeDefinition>typeConstruct.typeRef.resolved;
                    let typePropDec = typeDef.scope.table.get(refDot.memberValue.refName);
                    if (typePropDec) {
                        let propValue = typePropDec.rvalue;
                        if (!propValue) {
                            for (let i = 0; i < typeConstruct.assignments.length; i++) {
                                if (
                                    typeConstruct.assignments[i].lvalue.refName ===
                                    (<AST.ASTName>typePropDec.lvalue).refName
                                ) {
                                    propValue = typeConstruct.assignments[i].rvalue;
                                    break;
                                }
                            }
                            if (!propValue) {
                                this.err

                                    .atWholeNode("Const expression can't reference an uninitialized property", refDot)
                                    .atWholeNode(
                                        "Property " +
                                            AST.exprToStr(refDot.memberValue) +
                                            " is not initialized in the type definition",
                                        typePropDec
                                    )
                                    .atWholeNode_PANIC(
                                        "...nor is it initialized in the const type construction",
                                        typeConstruct.resolved
                                    );
                            }
                        }
                        this.recurseResConstExpression(propValue, sourceExpr, scope);
                        expr.resolved = propValue.resolved;
                        break;
                    } else {
                        this.err.atWholeNode_PANIC(
                            "Property " +
                                AST.exprToStr(refDot.memberValue) +
                                " not found in " +
                                AST.exprToStr(refDot.rootValue),
                            refDot
                        );
                    }
                } else {
                    this.err.atWholeNode_PANIC(
                        AST.exprToStr(refDot.rootValue) +
                            " has no properties which can be accessed because it has the type " +
                            AST.exprToStr(this.typeOfExpression(refDot.rootValue.resolved, scope)),
                        refDot.rootValue
                    );
                }
                let refDecl = accessScope.table.get(refDot.memberValue.refName);
                if (refDecl) {
                    if (
                        !refDecl.rvalue ||
                        !refDecl.accessAssignment ||
                        refDecl.accessAssignment.type !== TokenType.CONST_ASSIGNMENT_TKN
                    ) {
                        this.err
                            .atWholeNode("Constant expression is attempting to access non-constant values", refDot)
                            .atWholeNode("In " + AST.exprToStr(rootDec.lvalue) + "...", refDot.rootValue.resolved)
                            .atWholeNode_PANIC(
                                "Attempted to access " + AST.exprToStr(refDot.memberValue) + " which is not constant",
                                refDecl
                            );
                    }
                    this.recurseResConstExpression(refDecl.rvalue, sourceExpr, scope);
                    expr.resolved = refDecl.rvalue.resolved;
                    break;
                } else {
                    this.err.atWholeNode_PANIC(
                        "Property " +
                            AST.exprToStr(refDot.memberValue) +
                            " not found in " +
                            AST.exprToStr(refDot.rootValue),
                        refDot
                    );
                }
            case AST.NodeType.TYPE_CONSTRUCT:
                let astConstruct = <AST.ASTTypeConstruction>expr;
                // TODO type check the type construct. Similar to "typeOfTypeConstruct" function
                this.recurseResConstExpression(astConstruct.typeRef, sourceExpr, scope);
                if (astConstruct.typeRef.resolved.nodeName === AST.NodeType.TYPE_LITERAL) {
                    this.err.atWholeNode_PANIC(
                        "Can't construct a " +
                            AST.exprToStr(astConstruct.typeRef.resolved) +
                            " using a type constructor",
                        astConstruct.typeRef
                    );
                }
                if (astConstruct.typeRef.resolved.nodeName !== AST.NodeType.TYPE_DEF) {
                    this.err.atWholeNode_PANIC(
                        "Can't construct type for unknown type '" + AST.exprToStr(astConstruct.typeRef.resolved) + "'",
                        astConstruct.typeRef
                    );
                }
                for (let i = 0; i < astConstruct.assignments.length; i++) {
                    this.recurseResConstExpression(astConstruct.assignments[i].rvalue, sourceExpr, scope);
                }
                expr.resolved = expr;
                break;
            case AST.NodeType.CALL:
                throw "Unimplemented constant resolution of call: Requires evaluation stuff";
            case AST.NodeType.LITERAL:
                expr.resolved = expr;
                break;
            case AST.NodeType.BINARY_OP:
                let astBinOp = <AST.ASTBinaryOperator>expr;
                this.recurseResConstExpression(astBinOp.lvalue, sourceExpr, scope);
                this.recurseResConstExpression(astBinOp.rvalue, sourceExpr, scope);
                // TODO evaluate the binary expression if its possible
                expr.resolved = expr;
                break;
            default:
                this.err.atWholeNode_PANIC("Expected a constant but got " + AST.exprToStr(expr) + " instead", expr);
        }
    }
    private assertPureFunction(func: AST.ASTFunction, srcFunc: AST.ASTFunction) {
        this.assertPureBlock(func.block, srcFunc);
    }
    private assertPureBlock(block: AST.ASTBlock, srcFunc: AST.ASTFunction) {
        let statements = block.statements;
        for (let i = 0; i < statements.length; i++) {
            switch (statements[i].nodeName) {
                case AST.NodeType.IF:
                    this.assertPureIf(<AST.ASTIf>statements[i], srcFunc);
                    break;
                case AST.NodeType.WHILE:
                    let astWhile = <AST.ASTWhile>statements[i];
                    this.assertExprInPureFunc(astWhile.condition, block.scope, srcFunc);
                    this.assertPureBlock(astWhile.block, srcFunc);
                    break;
                case AST.NodeType.BREAK:
                case AST.NodeType.CONTINUE:
                    break;
                case AST.NodeType.RETURN:
                    let astRet = <AST.ASTReturn>statements[i];
                    this.assertExprInPureFunc(astRet.returnValue, block.scope, srcFunc);
                    break;
                case AST.NodeType.DECLARATION:
                    let astDecl = <AST.ASTDeclaration>statements[i];
                    let ldeclCompare = this.searchDeclarationName(<AST.ASTName>astDecl.lvalue, srcFunc.block.scope);
                    if (ldeclCompare) {
                        if (ldeclCompare.inScope.scopeID < srcFunc.block.scope.scopeID) {
                            // 1/6/21
                            this.err
                                .atNode("Error in pure function", srcFunc)
                                .atNode(
                                    "Can't assign a value to a variable outside of a pure function's scope",
                                    statements[i]
                                )
                                .atNode_PANIC("Outside declaration found here", ldeclCompare);
                        }
                    }
                    if (astDecl.rvalue && astDecl.rvalue.nodeName !== AST.NodeType.FUNCTION) {
                        this.assertExprInPureFunc(astDecl.rvalue, astDecl.inScope, srcFunc);
                    }
                    break;
                default:
                    throw AST.NodeType[statements[i].nodeName] + " constant checking not implemented yet";
            }
        }
        return block;
    }
    private assertPureIf(iff: AST.ASTIf, srcFunc: AST.ASTFunction) {
        this.assertExprInPureFunc(iff.condition, iff.consequence.scope.parent, srcFunc);
        this.assertPureBlock(iff.consequence, srcFunc);
        if (iff.alternative) {
            if (iff.alternative.nodeName === AST.NodeType.BLOCK) {
                this.assertPureBlock(<AST.ASTBlock>iff.alternative, srcFunc);
            } else {
                this.assertPureIf(<AST.ASTIf>iff.alternative, srcFunc);
            }
        }
    }
    private assertExprInPureFunc(expr: AST.ASTExpression, exprScope: Scope, srcFunc: AST.ASTFunction) {
        switch (expr.nodeName) {
            case AST.NodeType.NAME:
                let name = <AST.ASTName>expr;
                let nameDecl = this.searchDeclarationName(name, exprScope);
                if (nameDecl) {
                    if (nameDecl.inScope.scopeID < srcFunc.block.scope.scopeID) {
                        // 1/6/21
                        if (nameDecl.accessAssignment.type !== TokenType.CONST_ASSIGNMENT_TKN) {
                            this.err
                                .atNode("Error in pure function", srcFunc)
                                .atNode("Can't assign a value to a variable outside of a pure function's scope", expr)
                                .atNode_PANIC("Outside declaration found here", nameDecl);
                        }
                    }
                }
                break;
            case AST.NodeType.TYPE_CONSTRUCT:
                throw "Unimplemented purity check for type constructors";
            case AST.NodeType.CALL:
                let funcCall = <AST.ASTCall>expr;
                let funcRef = <AST.ASTName>funcCall.functionNameRef;
                throw "Unimplemented purity check for function calls";
            /*
                if (!funcRef) throw "Func call does not have a reference name";
                let funcDecl = this.searchVarDeclaration(<AST.ASTName>funcRef, funcScope);
                if (funcDecl) {
                    if (funcDecl.inScope.scopeID < funcScope.scopeID) {
                        // 1/6/21
                        if (funcDecl.accessAssignment.type !== TokenType.CONST_ASSIGNMENT_TKN) {
                            this.err.atNode_PANIC(
                                "Cannot call non-constant stuff from outside of a pure function's scope. That's impure",
                                expr
                            );
                        }
                    }
                }
                break;
                */
            case AST.NodeType.LITERAL:
                // you fine
                break;
            case AST.NodeType.BINARY_OP:
                let binOp = <AST.ASTBinaryOperator>expr;
                this.assertExprInPureFunc(binOp.lvalue, exprScope, srcFunc);
                this.assertExprInPureFunc(binOp.rvalue, exprScope, srcFunc);
                break;
            case AST.NodeType.UNARY_OP:
                this.assertExprInPureFunc((<AST.ASTUnaryOperator>expr).value, exprScope, srcFunc);
                break;
            default:
                throw AST.exprToStr(expr) + " not implemented yet (assertExprInPureFunc)";
        }
    }
    // Given an expression "3 + 6 + add(3, 4)" output the type "num"
    // Should proceed to perform all type checking and inference procedures while traversing the expression tree
    // Use function getConstExpression for constant folding stuff
    private typeOfExpression(expr: AST.ASTExpression, scope: Scope): AST.ASTExpression {
        switch (expr.nodeName) {
            case AST.NodeType.ACCESS_CAST:
                let accessCast = <AST.ASTAccessCast>expr;
                this.resolveTypeExpression(accessCast.castedType, scope);
                let accessCastType = new AST.ASTTypeLiteral(expr.locToken);
                accessCastType.type = TokenType.TYPE_TKN;
                return accessCastType;
            case AST.NodeType.TYPE_LITERAL:
            case AST.NodeType.FUNCTION_TYPE:
                let funcTypeLitType = new AST.ASTTypeLiteral(expr.locToken);
                funcTypeLitType.type = TokenType.TYPE_TKN;
                return funcTypeLitType;
            case AST.NodeType.MODULE:
                this.typeCheckModule(<AST.ASTModule>expr);
                let modType = new AST.ASTTypeLiteral(expr.locToken);
                modType.type = TokenType.MODULE_TKN;
                return modType;
            case AST.NodeType.TYPE_DEF:
                this.typeCheckTypeDefinition(<AST.ASTTypeDefinition>expr);
                let typeDefType = new AST.ASTTypeLiteral(expr.locToken);
                typeDefType.type = TokenType.TYPE_TKN;
                return typeDefType;
            case AST.NodeType.FUNCTION:
                let astFunc = <AST.ASTFunction>expr;
                this.typeCheckFunction(astFunc); //Type checks both the parameters and inner block statements
                let funcType = new AST.ASTFunctionType(astFunc.locToken);
                funcType.inputType = [];
                for (let i = 0; i < astFunc.paramDeclaration.length; i++) {
                    funcType.inputType.push(astFunc.paramDeclaration[i].type.resolved);
                }
                if (astFunc.returnType) {
                    this.resolveConstExpression(astFunc.returnType, scope);
                    funcType.outType = astFunc.returnType.resolved;
                } else {
                    funcType.outType = new AST.ASTTypeLiteral(funcType.locToken);
                    funcType.outType.type = TokenType.VOID_TYPE_TKN;
                }
                return funcType;
            case AST.NodeType.NAME:
                return this.typeOfName(<AST.ASTName>expr, scope);
            case AST.NodeType.DOT_OP:
                throw "Cry... ;-; (Dot operator not yet implemented)";
            case AST.NodeType.CALL:
                throw "Unimplemented type of for call";
            // let astCall = <AST.ASTCall>expr;
            // let paramTypes: AST.ASTExpression[] = [];
            // for (let i = 0; i < astCall.givenParams.length; i++) {
            //     paramTypes.push(this.typeOfExpression(astCall.givenParams[i], scope));
            // }
            // let funcRef = astCall.functionNameRef;
            // if (funcRef.nodeName !== AST.NodeType.NAME) throw "have not implemented general function call yet";

            // let funcDecl = this.searchDeclarationName(<AST.ASTName>astCall.functionNameRef, paramTypes, scope);
            // if (!funcDecl) throw "function dec doesn't exist";
            // let func = <AST.ASTFunction>funcDecl.rvalue;
            // let type = new AST.ASTTypeLiteral(func.locToken);
            // type.type = TokenType.NUM_TYPE_TKN;
            // this.resolveConstExpression(func.returnType, scope);
            // return this.castAccess(func.returnType.resolved, TokenType.MUT_CAST_TKN);
            case AST.NodeType.TYPE_CONSTRUCT:
                let astTypeCons = <AST.ASTTypeConstruction>expr;
                this.typeCheckTypeConstruction(astTypeCons, scope);
                return this.castAccess(astTypeCons.typeRef.resolved, TokenType.MUT_CAST_TKN);
            case AST.NodeType.LITERAL:
                let astLiteral = <AST.ASTLiteral>expr;
                let astLitNewTypeLit = new AST.ASTTypeLiteral(expr.locToken);
                switch (astLiteral.value.type) {
                    case TokenType.NUMERIC_LITERAL_TKN:
                        astLitNewTypeLit.type = TokenType.NUM_TYPE_TKN;
                        break;
                    case TokenType.STRING_LITERAL_TKN:
                        astLitNewTypeLit.type = TokenType.STRING_TYPE_TKN;
                        break;
                    case TokenType.COND_TRUE_TKN:
                    case TokenType.COND_FALSE_TKN:
                        astLitNewTypeLit.type = TokenType.BOOL_TYPE_TKN;
                        break;
                    default:
                        throw "The ASTLiteral does not contain a literal token";
                }
                return this.castAccess(astLitNewTypeLit, TokenType.MUT_CAST_TKN);
            case AST.NodeType.UNARY_OP:
                let astUnary = <AST.ASTUnaryOperator>expr;
                let unaryOp = astUnary.operation.type;
                let innerType = this.typeOfExpression(astUnary.value, scope);
                if (unaryOp == TokenType.BIN_NOT_TKN) {
                    // TODO -> Also consider case where the operator is overloaded to act on other types
                    if (this.isEqualToUncastedType(innerType, TokenType.NUM_TYPE_TKN)) {
                        let typeLiteral = new AST.ASTTypeLiteral(expr.locToken);
                        typeLiteral.type = TokenType.NUM_TYPE_TKN;
                        return typeLiteral;
                    }
                    throw "operator is acting on not an integer";
                } else if (unaryOp == TokenType.COND_NOT_TKN) {
                    // TODO -> Also consider case where the operator is overloaded to act on other types
                    if (this.isEqualToUncastedType(innerType, TokenType.BOOL_TYPE_TKN)) {
                        let typeLiteral = new AST.ASTTypeLiteral(expr.locToken);
                        typeLiteral.type = TokenType.BOOL_TYPE_TKN;
                        return typeLiteral;
                    }
                    throw "operator is acting on not a boolean";
                } else {
                    // TODO -> when this is implemented with operator keyword, it needs a final elseif / default condition
                    //         to go through all those other operators that may have been created in a loop
                    throw "operator doesn't exist";
                }
            case AST.NodeType.BINARY_OP:
                return this.typeOfBinaryOperator(<AST.ASTBinaryOperator>expr, scope);
            default:
                throw "You done fucked up";
        }
    }
    private typeOfNameStack: AST.ASTExpression[] = [];
    private typeOfName(exprName: AST.ASTName, scope: Scope): AST.ASTExpression {
        let nameDecl = this.searchDeclarationName(exprName, scope);
        if (!nameDecl) {
            this.err.atNode("Couldn't find variable with name '" + exprName.refName + "'", exprName);
            if (scope.getEnclosingScope(ScopeType.MODULE_SCOPE)) {
                this.err.addNote_PANIC("Modules can only access constant variables outside of their scope");
            }
            if (scope.getEnclosingScope(ScopeType.TYPE_SCOPE)) {
                this.err.addNote_PANIC("Type definitions can only access constant variables outside of their scope");
            }
            this.err.panic();
        }
        if (nameDecl.nodeName === AST.NodeType.FUNCTION) throw "Functions aren't implemented here yet...";

        if (
            !nameDecl.inScope.isUnordered() &&
            nameDecl.accessAssignment.type !== TokenType.CONST_ASSIGNMENT_TKN &&
            exprName.locToken.isTokenBefore(nameDecl.locToken)
        ) {
            this.err
                .atWholeToken("Variable is used before it is declared", exprName.locToken)
                .atNode("Declaration for the variable found here", nameDecl)
                .addNote_PANIC(
                    "Forward referencing for immutable and mutable variables is not allowed in block scopes (if, while, for, etc.)"
                );
        }
        for (let i = this.typeOfNameStack.length - 1; i >= 0; i--) {
            if (exprName.locToken.equals(this.typeOfNameStack[i].locToken)) {
                this.err.atNode("Cyclic dependency found when type checking the following", exprName);
                for (let j = i + 1; j < this.typeOfNameStack.length; j++) {
                    this.err.atNode("...which led to type checking the following", this.typeOfNameStack[j]);
                }
                this.err.panic();
            }
        }
        this.typeOfNameStack.push(exprName);
        this.typeCheckDeclaration(nameDecl);
        this.typeOfNameStack = [];

        // See 1/7/21 - Fixing distinction between pass by value and reference
        // If the found variable is related to a known type literal: num, string, bool or functionType,
        // copy the valuable of the variable and cast it to a mutable value
        if (!this.checkAccessCast(nameDecl.type.resolved, TokenType.MUT_CAST_TKN)) {
            let resolvedType = nameDecl.type.resolved;
            if (resolvedType.nodeName === AST.NodeType.ACCESS_CAST) {
                resolvedType = (<AST.ASTAccessCast>resolvedType).castedType;
            }
            if (
                resolvedType.nodeName === AST.NodeType.TYPE_LITERAL ||
                resolvedType.nodeName === AST.NodeType.FUNCTION_TYPE
            ) {
                return this.castAccess(resolvedType, TokenType.MUT_CAST_TKN);
            }
        }
        return nameDecl.type.resolved;
    }
    private typeOfBinaryOperator(binaryOp: AST.ASTBinaryOperator, scope: Scope): AST.ASTExpression {
        let lType = this.typeOfExpression(binaryOp.lvalue, scope);
        let rType = this.typeOfExpression(binaryOp.rvalue, scope);
        switch (binaryOp.operation.type) {
            case TokenType.COND_OR_TKN:
            case TokenType.COND_AND_TKN:
            case TokenType.COND_XOR_TKN:
            case TokenType.COND_EQUALS_TKN: // Needs to be expanded for num, string, bool, type, etc...
            case TokenType.COND_NOT_EQUALS_TKN: // Same thing as cond_equals
            case TokenType.COND_LESS_THAN_TKN:
            case TokenType.COND_LESS_THAN_EQUAL_TKN:
            case TokenType.COND_GREATER_THAN_TKN:
            case TokenType.COND_GREATER_THAN_EQUAL_TKN:
                if (this.isEqualToUncastedType(lType, TokenType.BOOL_TYPE_TKN)) {
                    if (this.isEqualToUncastedType(rType, TokenType.BOOL_TYPE_TKN)) {
                        let typeLit = new AST.ASTTypeLiteral(binaryOp.locToken);
                        typeLit.type = <AST.TypeLiteralToken>TokenType.BOOL_TYPE_TKN;
                        return this.castAccess(typeLit, TokenType.MUT_CAST_TKN);
                    } else {
                        this.err
                            .atWholeNode("Error while type checking operator", binaryOp)
                            .atWholeNode(
                                "Right hand side of operator expected to be of type bool but got " +
                                    AST.exprToStr(rType) +
                                    " instead",
                                binaryOp.rvalue
                            )
                            .addNote_PANIC("No other matching operator overloads");
                    }
                } else {
                    this.err
                        .atWholeNode("Error while type checking operator", binaryOp)
                        .atWholeNode(
                            "Left hand side of operator expected to be of type bool but got " +
                                AST.exprToStr(lType) +
                                " instead",
                            binaryOp.lvalue
                        )
                        .addNote_PANIC("No other matching operator overloads");
                }
            case TokenType.BIN_OR_TKN:
            case TokenType.BIN_AND_TKN:
            case TokenType.BIN_XOR_TKN:
            case TokenType.BIN_SHIFT_RIGHT_TKN:
            case TokenType.BIN_SHIFT_ARITHMETIC_RIGHT_TKN:
            case TokenType.BIN_SHIFT_LEFT_TKN:
            case TokenType.OP_ADD_TKN:
            case TokenType.OP_SUBTR_TKN:
            case TokenType.OP_MULT_TKN:
            case TokenType.OP_DIVIDE_TKN:
            case TokenType.OP_CARROT_TKN:
            case TokenType.OP_MODULUS_TKN:
            case TokenType.OP_SUBTR_TKN:
                if (this.isEqualToUncastedType(lType, TokenType.NUM_TYPE_TKN)) {
                    if (this.isEqualToUncastedType(rType, TokenType.NUM_TYPE_TKN)) {
                        let typeLit = new AST.ASTTypeLiteral(binaryOp.locToken);
                        typeLit.type = <AST.TypeLiteralToken>TokenType.NUM_TYPE_TKN;
                        return this.castAccess(typeLit, TokenType.MUT_CAST_TKN);
                    } else {
                        this.err
                            .atWholeNode("Error while type checking operator", binaryOp)
                            .atWholeNode(
                                "Right hand side of operator expected to be of type num but got " +
                                    AST.exprToStr(rType) +
                                    " instead",
                                binaryOp.rvalue
                            )
                            .addNote_PANIC("No other matching operator overloads");
                    }
                } else {
                    this.err
                        .atWholeNode("Error while type checking operator", binaryOp)
                        .atWholeNode(
                            "Left hand side of operator expected to be of type num but got " +
                                AST.exprToStr(lType) +
                                " instead",
                            binaryOp.lvalue
                        )
                        .addNote_PANIC("No other matching operator overloads");
                }
            default:
                throw "Unknown binary operator";
        }
    }
    // Attempts to find the declaration given a name and an optional set of parameters
    private searchDeclarationName(name: AST.ASTName, scope: Scope): AST.ASTDeclaration | undefined {
        if (!scope) return undefined;
        let outerDecl = this.searchDeclarationName(name, scope.parent); //Find outermost declaration

        let decl = scope.table.get(name.refName);
        if (!outerDecl) return decl;

        //While backtracking, if we encounter a type or module scope, then discard everything mutable and immutable in the outer scope
        if (scope.scopeType === ScopeType.MODULE_SCOPE || scope.scopeType === ScopeType.TYPE_SCOPE) {
            if (!outerDecl.accessAssignment || outerDecl.accessAssignment.type !== TokenType.CONST_ASSIGNMENT_TKN) {
                return undefined;
            }
        }

        if (!decl) return outerDecl;

        if (outerDecl.inScope.isUnordered() || outerDecl.locToken.isTokenBefore(decl.locToken)) {
            return outerDecl;
        } else {
            return decl;
        }
    }
    // Goal is to traverse the completed AST, do type checking and attempt to type infer whatever is needed
    typeCheckProgram(prgm: AST.ASTProgram) {
        for (let i = 0; i < prgm.declarations.length; i++) {
            this.typeCheckDeclaration(prgm.declarations[i]);
        }
    }
    private typeCheckBlock(block: AST.ASTBlock, expectedReturnType: AST.ASTExpression | undefined) {
        for (let i = 0; i < block.statements.length; i++) {
            let statement = block.statements[i];
            switch (statement.nodeName) {
                //while
                //for item, index in array {}
                //operator definition
                case AST.NodeType.IF:
                    this.typeCheckIf(<AST.ASTIf>statement, expectedReturnType, block.scope);
                    break;
                case AST.NodeType.WHILE:
                    let astWhile = <AST.ASTWhile>statement;
                    let whileCondition = this.typeOfExpression(astWhile.condition, block.scope);
                    if (!this.isEqualToUncastedType(whileCondition, TokenType.BOOL_TYPE_TKN)) {
                        this.err.atNode_PANIC(
                            "While statement condition expected to be a boolean but found " +
                                AST.exprToStr(whileCondition) +
                                " instead",
                            astWhile.condition
                        );
                    }
                    this.typeCheckBlock(astWhile.block, expectedReturnType);
                    break;
                case AST.NodeType.FOR:
                    let astFor = <AST.ASTFor>statement;
                    let indexDecl = this.searchDeclarationName(<AST.ASTName>astFor.indexParamDec.lvalue, block.scope);
                    if (indexDecl) {
                        this.err
                            .atWholeNode(
                                "Can't redeclare variable " + AST.exprToStr(indexDecl.lvalue) + " in for loop",
                                astFor.indexParamDec
                            )
                            .atWholeNode_PANIC("Other declaration already exists here", indexDecl);
                    }
                    let forLower = this.typeOfExpression(astFor.lowerBound, block.scope);
                    if (!this.isEqualToUncastedType(forLower, TokenType.NUM_TYPE_TKN)) {
                        this.err.atNode_PANIC(
                            "For loop lower bound range expected to be a number but found " +
                                AST.exprToStr(forLower) +
                                " instead",
                            forLower
                        );
                    }
                    let forUpper = this.typeOfExpression(astFor.upperBound, block.scope);
                    if (!this.isEqualToUncastedType(forUpper, TokenType.NUM_TYPE_TKN)) {
                        this.err.atNode_PANIC(
                            "For loop upper bound range expected to be a number but found " +
                                AST.exprToStr(this.typeOfExpression(forUpper, block.scope)) +
                                " instead",
                            forUpper
                        );
                    }
                    throw "unimplemented for loop condition";
                case AST.NodeType.BREAK:
                case AST.NodeType.CONTINUE:
                    break;
                case AST.NodeType.RETURN:
                    let astReturn = <AST.ASTReturn>statement;
                    if (expectedReturnType) {
                        if (!astReturn.returnValue) {
                            this.err
                                .atWholeNode(
                                    "Enclosing function has return type of " + AST.exprToStr(expectedReturnType),
                                    expectedReturnType
                                )
                                .atNode_PANIC(
                                    "Return statement expected to return an expression but instead got nothing",
                                    astReturn
                                );
                        }
                        let returnValType = this.typeOfExpression(astReturn.returnValue, block.scope);
                        if (this.checkAccessCast(expectedReturnType, TokenType.CONST_CAST_TKN)) {
                            this.resolveConstExpression(returnValType, block.scope);
                            returnValType = this.castAccess(returnValType.resolved, TokenType.CONST_CAST_TKN);
                        } else if (this.checkAccessCast(expectedReturnType, TokenType.IMMUT_CAST_TKN)) {
                            returnValType = this.castAccess(returnValType, TokenType.IMMUT_CAST_TKN);
                        } else if (this.checkAccessCast(expectedReturnType, TokenType.MUT_CAST_TKN)) {
                            if (this.checkAccessCast(returnValType, TokenType.CONST_CAST_TKN)) {
                                this.err
                                    .atWholeNode(
                                        "Return type of function is of type " + AST.exprToStr(expectedReturnType),
                                        expectedReturnType
                                    )
                                    .atWholeNode(
                                        "Const reference to a type instance can't be casted to a mutable type",
                                        astReturn.returnValue
                                    )
                                    .addNote_PANIC(
                                        "Casting the reference to a mutable type would enable modification of the original const variable"
                                    );
                            }
                            if (this.checkAccessCast(returnValType, TokenType.IMMUT_CAST_TKN)) {
                                this.err
                                    .atWholeNode(
                                        "Return type of function is of type " + AST.exprToStr(expectedReturnType),
                                        expectedReturnType
                                    )
                                    .atWholeNode(
                                        "Immutable reference to a type instance can't be casted to a mutable type",
                                        astReturn.returnValue
                                    )
                                    .addNote_PANIC(
                                        "Casting the reference to a mutable type would enable modification of the original immutable variable"
                                    );
                            }
                            returnValType = this.castAccess(returnValType, TokenType.MUT_CAST_TKN);
                        }
                        if (!this.isTypeEqual(expectedReturnType, returnValType, block.scope)) {
                            this.err
                                .atWholeNode(
                                    "Mismatch between function return type and return statement expression with type " +
                                        AST.exprToStr(returnValType),
                                    returnValType
                                )
                                .atWholeNode_PANIC(
                                    "Function return type expected to be " + AST.exprToStr(expectedReturnType),
                                    expectedReturnType
                                );
                        }
                    } else {
                        if (astReturn.returnValue) {
                            this.err.atWholeNode_PANIC(
                                "Function has implicit return type of void. Expected to return with no expression but found the following",
                                astReturn.returnValue
                            );
                        }
                    }
                    break;
                case AST.NodeType.DECLARATION:
                    this.typeCheckDeclaration(<AST.ASTDeclaration>statement);
                    break;
                default:
                    throw "Unimplemented type checking in block";
            }
        }
    }
    private typeCheckIf(iff: AST.ASTIf, expectedReturnType: AST.ASTExpression | undefined, scope: Scope) {
        let ifConditionType = this.typeOfExpression(iff.condition, scope);
        if (this.isEqualToUncastedType(ifConditionType, TokenType.BOOL_TYPE_TKN)) {
            if (iff.consequence) this.typeCheckBlock(iff.consequence, expectedReturnType);
            if (iff.alternative) {
                if (iff.alternative.nodeName === AST.NodeType.BLOCK) {
                    this.typeCheckBlock(<AST.ASTBlock>iff.alternative, expectedReturnType);
                } else {
                    this.typeCheckIf(<AST.ASTIf>iff.alternative, expectedReturnType, scope);
                }
            }
        } else {
            this.err.atNode_PANIC(
                "If statement conditional must be a boolean. Instead found " + AST.exprToStr(ifConditionType),
                iff.condition
            );
        }
    }
    // Goal is to type check and/or infer the declaration type
    private typeCheckDeclaration(decl: AST.ASTDeclaration) {
        if (decl.hasTypeChecked) return; // Declaration has already been type checked so don't do it again

        if (decl.type) this.resolveTypeExpression(decl.type, decl.inScope);
        if (decl.rvalue) {
            if (decl.rvalue.nodeName === AST.NodeType.MODULE) {
                if (decl.lvalue.nodeName !== AST.NodeType.NAME) {
                    this.err.atNode_PANIC("Module declaration must declare a new single variable name", decl);
                }
                if (decl.accessAssignment.type !== TokenType.CONST_ASSIGNMENT_TKN) {
                    this.err.atToken_PANIC(
                        "Module declarations must be constant: " + AST.exprToStr(decl.lvalue) + " => module {...}",
                        decl.accessAssignment
                    );
                }
            }
            if (decl.rvalue.nodeName === AST.NodeType.TYPE_DEF) {
                if (decl.lvalue.nodeName !== AST.NodeType.NAME) {
                    this.err.atNode_PANIC("Type definition must declare a new single variable name", decl);
                }
                if (decl.accessAssignment.type !== TokenType.CONST_ASSIGNMENT_TKN) {
                    this.err.atToken_PANIC(
                        "Type definitions must be constant: " + AST.exprToStr(decl.lvalue) + " => type {...}",
                        decl.accessAssignment
                    );
                }
            }
            let expressionType!: AST.ASTExpression;
            if (decl.accessAssignment.type === TokenType.CONST_ASSIGNMENT_TKN) {
                this.resolveConstExpression(decl.rvalue, decl.inScope);
                expressionType = this.castAccess(
                    this.typeOfExpression(decl.rvalue.resolved, decl.inScope),
                    TokenType.CONST_CAST_TKN
                );
            } else if (decl.accessAssignment.type === TokenType.IMMUTABLE_ASSIGNMENT_TKN) {
                expressionType = this.castAccess(
                    this.typeOfExpression(decl.rvalue, decl.inScope),
                    TokenType.IMMUT_CAST_TKN
                );
            } else if (decl.accessAssignment.type === TokenType.MUTABLE_ASSIGNMENT_TKN) {
                expressionType = this.typeOfExpression(decl.rvalue, decl.inScope);
                if (this.checkAccessCast(expressionType, TokenType.CONST_CAST_TKN)) {
                    this.err
                        .atWholeNode(
                            "The right side value of the declaration is of inferred type " +
                                AST.exprToStr(expressionType),
                            decl.rvalue
                        )
                        .atWholeNode(
                            "Constant reference to a type instance can't be assigned to a mutable variable",
                            decl
                        )
                        .addNote_PANIC(
                            "Casting the reference to a mutable type would enable modification of the original const variable"
                        );
                }
                if (this.checkAccessCast(expressionType, TokenType.IMMUT_CAST_TKN)) {
                    this.err
                        .atWholeNode(
                            "The right side value of the declaration is of inferred type " +
                                AST.exprToStr(expressionType),
                            decl.rvalue
                        )
                        .atWholeNode(
                            "Immutable reference to a type instance can't be assigned to a mutable variable",
                            decl
                        )
                        .addNote_PANIC(
                            "Casting the reference to a mutable type would enable modification of the original immutable variable"
                        );
                }
                expressionType = this.castAccess(expressionType, TokenType.MUT_CAST_TKN);
            }

            if (!decl.type) {
                decl.type = new AST.ASTTypeLiteral(decl.lvalue.locToken); //Arbritrary placeholder empty type
                decl.type.resolved = expressionType;
            } else {
                // variable: mut num ~= 3;
                if (this.checkAccessCast(decl.type.resolved, TokenType.CONST_CAST_TKN)) {
                    if (decl.accessAssignment.type !== TokenType.CONST_ASSIGNMENT_TKN) {
                        this.err
                            .atWholeToken("Mismatch between variable type and assignment type", decl.accessAssignment)
                            .addNote_PANIC(
                                "Use => to denote const assignment " +
                                    AST.exprToStr(decl.lvalue) +
                                    ": " +
                                    AST.exprToStr(decl.type) +
                                    " => " +
                                    AST.exprToStr(decl.rvalue) +
                                    ";"
                            );
                    }
                } else if (this.checkAccessCast(decl.type.resolved, TokenType.IMMUT_CAST_TKN)) {
                    if (decl.accessAssignment.type !== TokenType.IMMUTABLE_ASSIGNMENT_TKN) {
                        this.err
                            .atWholeToken("Mismatch between variable type and assignment type", decl.accessAssignment)
                            .addNote_PANIC(
                                "Use = to denote immutable assignment " +
                                    AST.exprToStr(decl.lvalue) +
                                    ": " +
                                    AST.exprToStr(decl.type) +
                                    " = " +
                                    AST.exprToStr(decl.rvalue) +
                                    ";"
                            );
                    }
                } else if (this.checkAccessCast(decl.type.resolved, TokenType.MUT_CAST_TKN)) {
                    if (decl.accessAssignment.type !== TokenType.MUTABLE_ASSIGNMENT_TKN) {
                        this.err
                            .atWholeToken("Mismatch between variable type and assignment type", decl.accessAssignment)
                            .addNote_PANIC(
                                "Use ~= to denote mut assignment " +
                                    AST.exprToStr(decl.lvalue) +
                                    ": " +
                                    AST.exprToStr(decl.type) +
                                    " ~= " +
                                    AST.exprToStr(decl.rvalue) +
                                    ";"
                            );
                    }
                }
                if (!this.isTypeEqual(decl.type.resolved, expressionType, decl.inScope)) {
                    this.err
                        .atNode(
                            "Type mismatch between declared type and expression type. The inferred declare type is " +
                                AST.exprToStr(decl.type.resolved),
                            decl.type
                        )
                        .atNode_PANIC(
                            "The expression has an inferred type of " + AST.exprToStr(expressionType),
                            decl.rvalue
                        );
                }
            }
        } else {
            if (decl.type.resolved.nodeName === AST.NodeType.ACCESS_CAST) {
                let cast = <AST.ASTAccessCast>decl.type.resolved;
                if (cast.accessType === TokenType.CONST_CAST_TKN) {
                    this.err.atNode_PANIC("Constant declarations must define a right side value", decl);
                } else {
                    decl.accessAssignment = Token.fromType(TokenType.MUTABLE_ASSIGNMENT_TKN, decl.type.locToken);
                }
            } else {
                decl.accessAssignment = Token.fromType(TokenType.IMMUTABLE_ASSIGNMENT_TKN, decl.type.locToken);
            }
        }

        // Handle checking for scopes, matchin declaration types to assignments and ensuing that access modifiers are adhered to
        let otherDecl = this.searchDeclarationName(<AST.ASTName>decl.lvalue, decl.inScope);
        // If the other declaration is the same as the current declaration, don't need to compare them
        if (otherDecl && !otherDecl.locToken.equals(decl.locToken)) {
            if (!decl.isAssignment) {
                decl.isAssignment = true;
                decl.inScope.table.delete((<AST.ASTName>decl.lvalue).refName);
            }
            if (decl.inScope.isUnordered() && decl.inScope.equals(otherDecl.inScope)) {
                if (decl.inScope.scopeType === ScopeType.GLOBAL_SCOPE) {
                    this.err
                        .atNode("Found duplicate variable declaration in global scope", decl)
                        .atNode("Other duplicate declaration found here", otherDecl)
                        .addNote_PANIC("Can't have duplicate variables in global scope");
                }
                this.err
                    .atNode("Found duplicate variable declaration", decl)
                    .atNode("Other duplicate declaration found here", otherDecl)
                    .addNote_PANIC("Can't have duplicate variables in modules or type definitions");
            }
            this.typeCheckDeclaration(otherDecl);
            if (otherDecl.accessAssignment.type === TokenType.CONST_ASSIGNMENT_TKN) {
                this.err
                    .atWholeToken("Reassignment of constant variable is not allowed", decl.accessAssignment)
                    .atWholeToken("Const declaration already exists here", otherDecl.locToken)
                    .addNote_PANIC("All constant variables are unordered within a scope");
            }
            if (otherDecl.rvalue && otherDecl.accessAssignment.type === TokenType.IMMUTABLE_ASSIGNMENT_TKN) {
                if (otherDecl.rvalue.nodeName === AST.NodeType.FUNCTION) {
                    this.err
                        .atWholeNode("Function overloading is not allowed", decl)
                        .atWholeNode_PANIC("Other function declaration already exists here", otherDecl);
                } else {
                    this.err
                        .atWholeNode("Reassignment of immutable variable is not allowed", decl)
                        .atWholeNode_PANIC("Immutable declaration already exists here", otherDecl);
                }
            }
            if (
                decl.type &&
                (decl.type.nodeName !== AST.NodeType.TYPE_LITERAL || (<AST.ASTTypeLiteral>decl.type).type)
            ) {
                this.err
                    .atNode("Duplicate variable declaration found", decl)
                    .atNode("Other variable found in outer scope", otherDecl)
                    .addNote_PANIC("Variable shadowing is not allowed in this language");
            }
            if (!this.isTypeEqual(otherDecl.type, decl.type, decl.inScope)) {
                this.err
                    .atNode(
                        "Type mismatch between variable assignment and declaration. Inferred type of assignment is " +
                            AST.exprToStr(decl.type.resolved),
                        decl
                    )
                    .atNode(
                        "Declaration found here with inferred type of " + AST.exprToStr(otherDecl.type.resolved),
                        otherDecl
                    );
                if (
                    otherDecl.type.resolved.nodeName === AST.NodeType.ACCESS_CAST &&
                    decl.type.resolved.nodeName === AST.NodeType.ACCESS_CAST
                ) {
                    this.err.addNote_PANIC("Must use consistent access type when assigning to a declared variable");
                } else {
                    this.err.panic();
                }
            }
        }
        decl.hasTypeChecked = true;
    }
    private typeCheckModule(module: AST.ASTModule) {
        for (let i = 0; i < module.declarations.length; i++) {
            this.typeCheckDeclaration(module.declarations[i]);
        }
    }
    private typeCheckTypeDefinition(typeDefinition: AST.ASTTypeDefinition) {
        for (let i = 0; i < typeDefinition.declarations.length; i++) {
            this.typeCheckDeclaration(typeDefinition.declarations[i]);
        }
    }
    private typeCheckFunction(func: AST.ASTFunction) {
        for (let i = 0; i < func.paramDeclaration.length; i++) {
            this.typeCheckDeclaration(func.paramDeclaration[i]);
        }
        if (func.returnType) {
            this.resolveConstExpression(func.returnType, func.block.scope);
            if (this.isEqualToUncastedType(func.returnType.resolved, TokenType.VOID_TYPE_TKN)) {
                this.typeCheckBlock(func.block, undefined);
            }
        }
        this.typeCheckBlock(func.block, func.returnType);
        if (func.returnType && !this.isEqualToUncastedType(func.returnType, TokenType.VOID_TYPE_TKN)) {
            if (!func.block.hasReturn) {
                this.err.atWholeNode_PANIC(
                    "A function with a return type must return a value for all cases",
                    func.returnType
                );
            }
        }
    }
    private typeCheckTypeConstructStack: AST.ASTExpression[] = [];
    private typeCheckTypeConstruction(typeConstruct: AST.ASTTypeConstruction, scope: Scope) {
        let typeConsAssignments = typeConstruct.assignments;

        for (let i = this.typeCheckTypeConstructStack.length - 1; i >= 0; i--) {
            if (typeConstruct.locToken.equals(this.typeCheckTypeConstructStack[i].locToken)) {
                this.err.atNode("Recursive type definition found when type checking the following", typeConstruct);
                for (let j = i + 1; j < this.typeCheckTypeConstructStack.length; j++) {
                    this.err.atNode(
                        "...which leads to the creation of the following",
                        this.typeCheckTypeConstructStack[j]
                    );
                }
                this.err.panic();
            }
        }
        this.typeCheckTypeConstructStack.push(typeConstruct);
        this.typeCheckTypeConstructStack.push(typeConstruct.typeRef);
        this.resolveConstExpression(typeConstruct.typeRef, scope);
        let constTypeName = typeConstruct.typeRef.resolved;
        if (constTypeName.nodeName !== AST.NodeType.TYPE_DEF) {
            this.err
                .atWholeNode("Type constuction does not refer to the creation of a type", typeConstruct.typeRef)
                .atWholeNode_PANIC(
                    AST.exprToStr(typeConstruct.typeRef) +
                        " evaluates to " +
                        AST.exprToStr(constTypeName) +
                        " which is not a valid type",
                    constTypeName
                );
        }
        let typeDef = <AST.ASTTypeDefinition>constTypeName;
        this.typeCheckTypeConstructStack = [];

        for (let i = 0; i < typeConsAssignments.length; i++) {
            let resolvedAssignmentType = this.typeOfExpression(typeConsAssignments[i].rvalue, scope);
            let assignmentVarName = (<AST.ASTName>typeConsAssignments[i].lvalue).refName;

            let typeVarDecl = undefined;
            for (let j = 0; j < typeDef.declarations.length; j++) {
                if ((<AST.ASTName>typeDef.declarations[j].lvalue).refName === assignmentVarName) {
                    typeVarDecl = typeDef.declarations[j];
                    this.typeCheckDeclaration(typeVarDecl); //Ensure that typeVarDecl has an accessAssignment
                    if (typeVarDecl.accessAssignment.type === TokenType.CONST_ASSIGNMENT_TKN) {
                        this.err
                            .atWholeNode("Error in type construction", typeConstruct)
                            .atToken(
                                "Attempting to assign a new value to an already constant variable",
                                typeConsAssignments[i].locToken
                            )
                            .atNode_PANIC("Constant variable in type definition found here", typeVarDecl);
                    }
                    break;
                }
            }
            if (typeVarDecl) {
                if (typeVarDecl.accessAssignment.type === TokenType.MUTABLE_ASSIGNMENT_TKN) {
                    if (this.checkAccessCast(resolvedAssignmentType, TokenType.IMMUT_CAST_TKN)) {
                        this.err
                            .atWholeNode("Error in type construction", typeConstruct)
                            .atNode(
                                "Variable assignment with inferred type of " + AST.exprToStr(resolvedAssignmentType),
                                typeConsAssignments[i].rvalue
                            )
                            .atNode(
                                "Immutable reference to a type instance can't be assigned to mutable variable found here",
                                typeVarDecl
                            )
                            .addNote_PANIC(
                                "Casting the reference to a mutable type would enable modification of the original immutable variable"
                            );
                    }
                } else if (typeVarDecl.accessAssignment.type === TokenType.IMMUTABLE_ASSIGNMENT_TKN) {
                    resolvedAssignmentType = this.castAccess(resolvedAssignmentType, TokenType.IMMUT_CAST_TKN);
                }

                if (!this.isTypeEqual(resolvedAssignmentType, typeVarDecl.type, scope)) {
                    this.err
                        .atWholeNode("Error in type construction", typeConstruct)
                        .atToken(
                            "Variable assignment with inferred type of " + AST.exprToStr(resolvedAssignmentType),
                            typeConsAssignments[i].locToken
                        )
                        .atNode_PANIC(
                            "...is incompatible with expected type of " + AST.exprToStr(typeVarDecl.type.resolved),
                            typeVarDecl
                        );
                }
            } else {
                this.err
                    .atNode("Error in type construction", typeConstruct)
                    .atNode("Property name '" + assignmentVarName + "' does not exist in type", typeConstruct.typeRef)
                    .atToken_PANIC("Invalid assignment found here", typeConsAssignments[i].locToken);
            }
        }
    }
}
