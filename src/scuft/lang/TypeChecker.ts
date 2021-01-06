import * as AST from "./Ast";
import { ErrorHandler } from "./ErrorHandler";
import Scope from "./Scope";
import { Token, TokenType } from "./Token";

export default class TypeChecker {
    private globalScope!: Scope;
    private readonly err: ErrorHandler;
    constructor(globalScope: Scope, errHandler: ErrorHandler) {
        this.globalScope = globalScope;
        this.err = ErrorHandler.fromHandler("----TypeChecker----\n", errHandler);
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
                    throw "can't cast const type to mutable"; // mut const
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
    // Converts an expression into a strict type "getConstNum()" evaluates to "num"  and where rand = num "rand" evaluates to "num"
    // May modify the parameter expression
    // May also attempt to validate the existence of types
    private resolveTypeExpression(typeExpr: AST.ASTExpression, scope: Scope): AST.ASTStrictType {
        switch (typeExpr.nodeName) {
            case AST.NodeType.ACCESS_CAST:
                let astAccessCast = <AST.ASTAccessCast>typeExpr;
                astAccessCast.castedType = this.resolveTypeExpression(astAccessCast.castedType, scope);
                return this.resolveAccessCast(astAccessCast);
            case AST.NodeType.TYPE_LITERAL:
                return <AST.ASTStrictType>typeExpr;
            case AST.NodeType.FUNCTION_TYPE:
                let astFunc = <AST.ASTFunctionType>typeExpr;
                astFunc.outType = this.resolveTypeExpression(astFunc.outType, scope);
                for (let i = 0; i < astFunc.inputType.length; i++) {
                    astFunc.inputType[i] = this.resolveTypeExpression(astFunc.inputType[i], scope);
                }
                return astFunc;
            case AST.NodeType.NAME:
                let astName = <AST.ASTName>typeExpr;
                let nameDeclaration = scope.searchVariableDeclaration(astName);
                if (nameDeclaration) {
                    if (nameDeclaration.rvalue) {
                        if (nameDeclaration.rvalue.nodeName === AST.NodeType.TYPE_DEF) {
                            return astName; //If the name is a type, return the type: For example: a: Particle has type "Particle";
                        } else {
                            throw "Unimplemented: Need to recurse to find correct type name";
                        }
                    } else {
                        throw "Type couldn't be found from name";
                    }
                } else {
                    this.err.atNode_PANIC("Couldn't find variable with name '" + astName.refName + "'", astName);
                }
            case AST.NodeType.CALL:
                throw "Evaluate the value of the call at run time, For example: a: getNumType() = 3";
            default:
                console.log(typeExpr);
                throw "Type expression was expected but not received";
        }
    }
    // Compares two type expressions (presumed to be types) and returns whether or not they are equivalent
    // "(num, num) -> num" and "(num, pureGetNumType()) -> num" returns true assuming that pureGetNumType returns "num"
    private isTypeEqual(t0: AST.ASTStrictType, t1: AST.ASTStrictType, scope: Scope): boolean {
        if (t0.nodeName !== t1.nodeName) return false;
        switch (t0.nodeName) {
            case AST.NodeType.ACCESS_CAST:
                let accessCast0 = <AST.ASTAccessCast>t0;
                let accessCast1 = <AST.ASTAccessCast>t1;
                if (accessCast0.accessType !== accessCast1.accessType) return false;
                let accessCastRes0 = this.resolveTypeExpression(accessCast0.castedType, scope);
                let accessCastRes1 = this.resolveTypeExpression(accessCast1.castedType, scope);
                return this.isTypeEqual(accessCastRes0, accessCastRes1, scope);
            case AST.NodeType.TYPE_LITERAL:
                let typeLit0 = <AST.ASTTypeLiteral>t0;
                let typeLit1 = <AST.ASTTypeLiteral>t1;
                return typeLit0.type === typeLit1.type;
            case AST.NodeType.FUNCTION_TYPE:
                let func0 = <AST.ASTFunctionType>t0;
                let func1 = <AST.ASTFunctionType>t1;

                let funcO0 = this.resolveTypeExpression(func0.outType, scope);
                let funcO1 = this.resolveTypeExpression(func1.outType, scope);
                if (!this.isTypeEqual(funcO0, funcO1, scope)) return false;
                for (let i = 0; i < func0.inputType.length; i++) {
                    let funcI0 = this.resolveTypeExpression(func0.inputType[i], scope);
                    let funcI1 = this.resolveTypeExpression(func1.inputType[i], scope);
                    if (!this.isTypeEqual(funcI0, funcI1, scope)) return false;
                }
                return true;
            case AST.NodeType.NAME:
                /*
                Particle = type{}
                Thing = Particle;
                a: Particle = Thing.{};  // This should type check
                */
                let name0 = <AST.ASTName>t0;
                let name1 = <AST.ASTName>t1;
                // TODO this is not completely correct
                // Need to check declaration rvalues
                return name0.refName === name1.refName;
            case AST.NodeType.DOT_OP:
                let dot0 = <AST.ASTDotOperator>t0;
                let dot1 = <AST.ASTDotOperator>t1;

                /* TODO this is not entirely correct. 
                Need to evaluate the dot operator to find the correct declaration and compare rvalues
                return (
                    this.isTypeExpressionEqual(dot0.rootValue, dot1.rootValue) &&
                    this.isTypeExpressionEqual(dot0.memberValue, dot1.memberValue)
                );*/
                return false;
            default:
                throw "This is an impossible case";
        }
    }
    private isEqualToType(typeExpr: AST.ASTExpression, typeLiteral: AST.TypeLiteralToken): boolean {
        if (typeExpr.nodeName === AST.NodeType.ACCESS_CAST) {
            return this.isEqualToType((<AST.ASTAccessCast>typeExpr).castedType, typeLiteral);
        }
        if (typeExpr.nodeName !== AST.NodeType.TYPE_LITERAL) return false;
        return (<AST.ASTTypeLiteral>typeExpr).type === typeLiteral;
    }
    // TODO check for cycles
    private isConstExpression(expr: AST.ASTExpression, scope: Scope): boolean {
        switch (expr.nodeName) {
            case AST.NodeType.TYPE_LITERAL:
            case AST.NodeType.FUNCTION_TYPE:
            case AST.NodeType.TYPE_DEF:
            case AST.NodeType.MODULE:
                return true;
            case AST.NodeType.FUNCTION:
                // TODO determine if function is const/pure
                return true;
            case AST.NodeType.NAME:
                let astName = <AST.ASTName>expr;
                // Entire scope is searchable because nameDecl is assumed to be constant
                let nameDecl = scope.searchVariableDeclaration(astName);
                if (nameDecl) {
                    this.typeCheckDeclaration(nameDecl);
                    if (nameDecl.resolvedType.nodeName === AST.NodeType.ACCESS_CAST) {
                        return (<AST.ASTAccessCast>nameDecl.resolvedType).accessType === TokenType.CONST_CAST_TKN;
                    } else {
                        return false;
                    }
                } else {
                    throw "Variable name does exist while doing constant check";
                }
            case AST.NodeType.TYPE_CONSTRUCT:
                // TODO must also ensure that the assignments are constant
                return true;
            case AST.NodeType.LITERAL:
                return true;
            default:
                throw "Unknown expression";
        }
    }
    // Given an expression "3 + 6 + add(3, 4)" output the type "num"
    // Should proceed to perform all type checking and inference procedures while traversing the expression tree
    private typeOfExpression(expr: AST.ASTExpression, scope: Scope): AST.ASTStrictType {
        switch (expr.nodeName) {
            case AST.NodeType.TYPE_LITERAL:
            case AST.NodeType.FUNCTION_TYPE:
                return AST.newTypeLiteralNode(TokenType.TYPE_TKN, expr.locToken);
            case AST.NodeType.MODULE:
                // TODO module type needs to be checked and inferred here
                return AST.newTypeLiteralNode(TokenType.MODULE_TKN, expr.locToken);
            case AST.NodeType.TYPE_DEF:
                this.typeCheckTypeDefinition(<AST.ASTTypeDefinition>expr);
                return AST.newTypeLiteralNode(TokenType.TYPE_TKN, expr.locToken);
            case AST.NodeType.FUNCTION:
                let astFunc = <AST.ASTFunction>expr;
                let funcType = new AST.ASTFunctionType(astFunc.locToken);
                funcType.inputType = [];
                for (let i = 0; i < astFunc.paramDeclaration.length; i++) {
                    this.typeCheckDeclaration(astFunc.paramDeclaration[i]);
                    funcType.inputType.push(astFunc.paramDeclaration[i].resolvedType);
                }
                if (astFunc.returnType) {
                    funcType.outType = this.resolveTypeExpression(astFunc.returnType, scope);
                } else {
                    funcType.outType = AST.newTypeLiteralNode(TokenType.VOID_TYPE_TKN, funcType.locToken);
                }
                this.typeCheckBlock(astFunc.block);
                return funcType;
            case AST.NodeType.NAME:
                return this.typeOfName(<AST.ASTName>expr, scope);
            case AST.NodeType.DOT_OP:
                let astDot = <AST.ASTDotOperator>expr;
                // root value
                // value of certain type
                // module
                throw "Cry... ;-; (Dot operator not yet implemented)";
            case AST.NodeType.CALL:
                let astCall = <AST.ASTCall>expr;
                let paramTypes: AST.ASTStrictType[] = [];
                for (let i = 0; i < astCall.givenParams.length; i++) {
                    paramTypes.push(this.typeOfExpression(astCall.givenParams[i], scope));
                }
                let outType = this.getFunctionDeclaration(astCall.functionNameRef, paramTypes, scope);
                if (outType) {
                    return this.resolveTypeExpression(outType.outType, scope);
                }
                throw "Unimplemented call, can't get type of call yet";
            case AST.NodeType.TYPE_CONSTRUCT:
                let astTypeCons = <AST.ASTTypeConstruction>expr;
                this.typeCheckTypeConstruction(astTypeCons, scope);
                return <AST.ASTName>astTypeCons.typeRef; // TODO Handle dot operator things like test.Particle.{}
            case AST.NodeType.LITERAL:
                let astLiteral = <AST.ASTLiteral>expr;
                switch (astLiteral.value.type) {
                    case TokenType.NUMERIC_LITERAL_TKN:
                        return AST.newTypeLiteralNode(TokenType.NUM_TYPE_TKN, expr.locToken);
                    case TokenType.STRING_LITERAL_TKN:
                        return AST.newTypeLiteralNode(TokenType.STRING_TYPE_TKN, expr.locToken);
                    case TokenType.COND_TRUE_TKN:
                    case TokenType.COND_FALSE_TKN:
                        return AST.newTypeLiteralNode(TokenType.BOOL_TYPE_TKN, expr.locToken);
                    default:
                        throw "The ASTLiteral does not contain a literal token";
                }
            case AST.NodeType.UNARY_OP:
                let astUnary = <AST.ASTUnaryOperator>expr;
                let unaryOp = astUnary.operation.type;
                if (unaryOp == TokenType.BIN_NOT_TKN) {
                    let numType = AST.newTypeLiteralNode(TokenType.NUM_TYPE_TKN, expr.locToken);
                    // TODO -> Also consider case where the operator is overloaded to act on other types
                    if (this.isTypeEqual(this.typeOfExpression(astUnary.value, scope), numType, scope)) {
                        return numType;
                    }
                    throw "operator is acting on not an integer";
                } else if (unaryOp == TokenType.COND_NOT_TKN) {
                    let boolType = AST.newTypeLiteralNode(TokenType.BOOL_TYPE_TKN, expr.locToken);
                    // TODO -> Also consider case where the operator is overloaded to act on other types
                    if (this.isTypeEqual(this.typeOfExpression(astUnary.value, scope), boolType, scope)) {
                        return boolType;
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
    private typeOfName(exprName: AST.ASTName, scope: Scope) {
        let nameDecl = scope.searchVariableDeclaration(exprName);
        if (nameDecl) {
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
            return nameDecl.resolvedType;
        } else {
            this.err.atNode_PANIC("Couldn't find variable with name '" + exprName.refName + "'", exprName);
        }
    }
    private getFunctionDeclaration(
        funcRef: AST.ASTExpression,
        parameterTypes: AST.ASTStrictType[],
        scope: Scope
    ): AST.ASTFunctionType {
        // TODO get this into a general form for not just ASTName reference
        if (funcRef.nodeName !== AST.NodeType.NAME) throw "have not implemented general function call yet";
        let funcName = <AST.ASTName>funcRef;
        let currentScopeDeclarations = scope.table.get(funcName.refName);
        if (currentScopeDeclarations) {
            for (let i = 0; i < currentScopeDeclarations.length; i++) {
                if (currentScopeDeclarations[i].rvalue.nodeName !== AST.NodeType.FUNCTION) {
                    throw "declaration name doesn't refer to a function";
                }
                this.typeCheckDeclaration(currentScopeDeclarations[i]);
                let funcSig = <AST.ASTFunctionType>currentScopeDeclarations[i].resolvedType;

                if (parameterTypes.length !== funcSig.inputType.length) {
                    continue;
                }
                for (let j = 0; j < funcSig.inputType.length; j++) {
                    let declParamType = this.resolveTypeExpression(funcSig.inputType[j], scope);
                    if (!this.isTypeEqual(parameterTypes[j], declParamType, scope)) break;
                    if (j + 1 == funcSig.inputType.length) return funcSig;
                }
            }
        }
        if (scope.parent) return this.getFunctionDeclaration(funcRef, parameterTypes, scope.parent);
        this.err.atNode_PANIC("Function not declared anywhere!!!!", funcRef);
    }
    private typeOfBinaryOperator(binaryOp: AST.ASTBinaryOperator, scope: Scope): AST.ASTStrictType {
        let lType = this.typeOfExpression(binaryOp.lvalue, scope);
        let rType = this.typeOfExpression(binaryOp.rvalue, scope);
        let overloads = this.globalScope.getOperatorOverload(binaryOp.operation.type);
        if (overloads) {
            for (let i = 0; i < overloads.length; i++) {
                if (overloads[i].paramDeclaration.length !== 2) {
                    this.err.atNode_PANIC("Binary operator overload must have exactly 2 parameters", overloads[i]);
                }
                if (!overloads[i].returnType) {
                    this.err.atNode_PANIC("Binary operator overload must have a return type", overloads[i]);
                } else if (
                    this.isEqualToType(
                        this.resolveTypeExpression(overloads[i].returnType, scope),
                        TokenType.VOID_TYPE_TKN
                    )
                ) {
                    this.err.atNode_PANIC(
                        "Binary operator overload can't have a void return type",
                        overloads[i].returnType
                    );
                }
                this.typeCheckDeclaration(overloads[i].paramDeclaration[0]);
                if (this.isTypeEqual(lType, overloads[i].paramDeclaration[0].resolvedType, scope)) {
                    this.typeCheckDeclaration(overloads[i].paramDeclaration[1]);
                    if (this.isTypeEqual(rType, overloads[i].paramDeclaration[1].resolvedType, scope)) {
                        return this.resolveTypeExpression(overloads[i].returnType, scope);
                    }
                }
            }
        }
        switch (binaryOp.operation.type) {
            case TokenType.COND_OR_TKN:
            case TokenType.COND_AND_TKN:
            case TokenType.COND_XOR_TKN:
            case TokenType.COND_EQUALS_TKN:
            case TokenType.COND_NOT_EQUALS_TKN:
            case TokenType.COND_LESS_THAN_TKN:
            case TokenType.COND_LESS_THAN_EQUAL_TKN:
            case TokenType.COND_GREATER_THAN_TKN:
            case TokenType.COND_GREATER_THAN_EQUAL_TKN:
                if (this.isEqualToType(lType, TokenType.BOOL_TYPE_TKN)) {
                    if (this.isEqualToType(rType, TokenType.BOOL_TYPE_TKN)) {
                        return lType;
                    } else {
                        this.err
                            .atToken("Error while type checking binary operation", binaryOp.operation)
                            .atNode(
                                "Right hand side of binary operator expected to be of type bool but got " +
                                    AST.exprToStr(rType) +
                                    " instead",
                                binaryOp.rvalue
                            )
                            .addNote_PANIC("No other matching operator overloads");
                    }
                } else {
                    this.err
                        .atToken("Error while type checking binary operation", binaryOp.operation)
                        .atNode(
                            "Left hand side of binary operator expected to be of type bool but got " +
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
                if (this.isEqualToType(lType, TokenType.NUM_TYPE_TKN)) {
                    if (this.isEqualToType(rType, TokenType.NUM_TYPE_TKN)) {
                        return lType;
                    } else {
                        this.err
                            .atToken("Error while type checking binary operation", binaryOp.operation)
                            .atNode(
                                "Right hand side of binary operator expected to be of type num but got " +
                                    AST.exprToStr(rType) +
                                    " instead",
                                binaryOp.rvalue
                            )
                            .addNote_PANIC("No other matching operator overloads");
                    }
                } else {
                    this.err
                        .atToken("Error while type checking binary operation", binaryOp.operation)
                        .atNode(
                            "Left hand side of binary operator expected to be of type num but got " +
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
    // Goal is to traverse the completed AST, do type checking and attempt to type infer whatever is needed
    typeCheckProgram(prgm: AST.ASTProgram) {
        for (let i = 0; i < prgm.declarations.length; i++) {
            this.typeCheckDeclaration(prgm.declarations[i]);
        }
    }
    private typeCheckBlock(block: AST.ASTBlock) {
        for (let i = 0; i < block.withModules.length; i++) {
            // TODO handle getting all the withModules
        }
        for (let i = 0; i < block.statements.length; i++) {
            let statement = block.statements[i];
            switch (statement.nodeName) {
                // TODO handle other statements like if, elseif, while and for
                case AST.NodeType.DECLARATION:
                    this.typeCheckDeclaration(<AST.ASTDeclaration>statement);
                    break;
                default:
                    throw "Unimplemented type checking in block";
            }
        }
    }
    private searchForExistingDeclaration(decl: AST.ASTDeclaration): AST.ASTDeclaration | undefined {
        let name = <AST.ASTName>decl.lvalue;

        let otherDecl = decl.inScope.searchVariableDeclaration(name, decl);
        if (otherDecl) {
            if (otherDecl.accessAssignment === TokenType.CONST_ASSIGNMENT_TKN) {
                this.err
                    .atNode("Reassignment of constant variable is not allowed", decl)
                    .atNode_PANIC("Const declaration already exists here", otherDecl);
            }
            if (otherDecl.accessAssignment === TokenType.IMMUTABLE_ASSIGNMENT_TKN) {
                this.err
                    .atNode("Reassignment of immutable variable is not allowed", decl)
                    .atNode_PANIC("Immutable declaration already exists here", otherDecl);
            }
            let isBefore =
                otherDecl.locToken.line < decl.locToken.line ||
                (otherDecl.locToken.line === decl.locToken.line && otherDecl.locToken.c < decl.locToken.c);
            if (otherDecl.inScope.unOrdered || isBefore) {
                if (decl.type) {
                    this.err
                        .atNode("Duplicate variable declaration found", decl)
                        .atNode("Other variable found in outer scope", otherDecl)
                        .addNote_PANIC("Variable shadowing is not allowed in this language");
                }
                decl.isAssignment = true;
                decl.inScope.removeDeclaration(decl);
                return otherDecl;
            } else {
                return this.searchForExistingDeclaration(otherDecl);
            }
        }
        return undefined;
    }
    private castAccess(expr: AST.ASTExpression, accessCast: AST.AccessCastToken) {
        let cast = new AST.ASTAccessCast(expr.locToken);
        cast.accessType = accessCast;
        cast.castedType = expr;
        return this.resolveAccessCast(cast);
    }
    // Goal is to type check and/or infer the declaration type
    private typeCheckDeclaration(decl: AST.ASTDeclaration) {
        if (decl.resolvedType) return; // Declaration has already been type checked so don't do it again

        let otherDec = this.searchForExistingDeclaration(decl);

        if (decl.rvalue) {
            if (decl.lvalue.nodeName !== AST.NodeType.NAME) {
                if (decl.rvalue.nodeName === AST.NodeType.MODULE) {
                    this.err.atNode_PANIC("Module declaration must declare a new single variable name", decl);
                }
                if (decl.rvalue.nodeName === AST.NodeType.TYPE_DEF) {
                    this.err.atNode_PANIC("Type definition must declare a new single variable name", decl);
                }
            }

            let expressionType = this.typeOfExpression(decl.rvalue, decl.inScope);
            if (expressionType.nodeName === AST.NodeType.FUNCTION_TYPE) {
                //throw "Functions are temporarily disabled in the type checker";
            }

            if (decl.accessAssignment === TokenType.CONST_ASSIGNMENT_TKN) {
                //variable => 3; or variable: num => 4;
                if (!this.isConstExpression(decl.rvalue, decl.inScope)) {
                    this.err
                        .atNode(
                            "Expected expression to be of a constant type. Instead got " +
                                AST.exprToStr(expressionType),
                            expressionType
                        )
                        .atNode_PANIC(
                            "Right hand side of this constant declaration is not a constant value",
                            decl.rvalue
                        );
                }
            }
            if (!decl.type) {
                // variable = 5;
                if (decl.accessAssignment === TokenType.IMMUTABLE_ASSIGNMENT_TKN) {
                    decl.resolvedType = expressionType;
                } else {
                    if (decl.accessAssignment === TokenType.CONST_ASSIGNMENT_TKN) {
                        decl.resolvedType = this.castAccess(expressionType, TokenType.CONST_CAST_TKN);
                    } else {
                        decl.resolvedType = this.castAccess(expressionType, TokenType.MUT_CAST_TKN);
                    }
                }
            } else {
                // variable: mut num ~= 3;
                decl.resolvedType = this.resolveTypeExpression(decl.type, decl.inScope);
                if (decl.resolvedType.nodeName === AST.NodeType.ACCESS_CAST) {
                    if ((<AST.ASTAccessCast>decl.resolvedType).accessType === TokenType.CONST_CAST_TKN) {
                        if (decl.accessAssignment !== TokenType.CONST_ASSIGNMENT_TKN) {
                            throw "Mismatch between variable type and assignment: Use => to denote const assignment";
                        }
                        expressionType = this.castAccess(expressionType, TokenType.CONST_CAST_TKN); // Promote expression to const
                    } else {
                        if (decl.accessAssignment !== TokenType.MUTABLE_ASSIGNMENT_TKN) {
                            throw "Mismatch between variable type and assignment: Use ~= to denote mut assignment";
                        }
                        expressionType = this.castAccess(expressionType, TokenType.MUT_CAST_TKN); // Demote expression to mut
                    }
                } else {
                    if (decl.accessAssignment !== TokenType.IMMUTABLE_ASSIGNMENT_TKN) {
                        throw "Mismatch between variable type and assignment: Use = to denote immutable assignment";
                    }
                }
                if (!this.isTypeEqual(expressionType, decl.resolvedType, decl.inScope)) {
                    this.err
                        .atNode(
                            "Type mismatch between declared type and expression type. The inferred declare type is " +
                                AST.exprToStr(decl.resolvedType),
                            decl.type
                        )
                        .atNode_PANIC(
                            "The inferred type of the expression is " + AST.exprToStr(expressionType),
                            decl.rvalue
                        );
                }
            }
        } else {
            decl.resolvedType = this.resolveTypeExpression(decl.type, decl.inScope);

            if (decl.resolvedType.nodeName === AST.NodeType.ACCESS_CAST) {
                let cast = <AST.ASTAccessCast>decl.resolvedType;
                if (cast.accessType === TokenType.CONST_CAST_TKN) {
                    this.err.atNode_PANIC("Constant declarations must define a right side value", decl);
                } else {
                    decl.accessAssignment = TokenType.MUTABLE_ASSIGNMENT_TKN;
                }
            } else {
                decl.accessAssignment = TokenType.IMMUTABLE_ASSIGNMENT_TKN;
            }
        }

        if (otherDec) {
            if (otherDec.accessAssignment !== TokenType.MUTABLE_ASSIGNMENT_TKN) {
                this.err
                    .atNode("Original declaration not mutable", otherDec)
                    .atNode_PANIC("Can't reassign declaration to assignment found here", decl);
            }
            decl.isAssignment = true;
            decl.inScope.removeDeclaration(decl);
            this.typeCheckDeclaration(otherDec);
            if (!this.isTypeEqual(otherDec.resolvedType, decl.resolvedType, decl.inScope)) {
                this.err
                    .atNode(
                        "Mismatched types between variable assignment and declaration. Inferred type of assignment is " +
                            AST.exprToStr(decl.resolvedType),
                        decl
                    )
                    .atNode(
                        "Declaration variable found here with inferred type " + AST.exprToStr(otherDec.resolvedType),
                        otherDec
                    );
                if (
                    otherDec.resolvedType.nodeName === AST.NodeType.ACCESS_CAST &&
                    decl.resolvedType.nodeName === AST.NodeType.ACCESS_CAST
                ) {
                    this.err.addNote_PANIC("Must use consistent access type when assigning to a declared variable");
                } else {
                    this.err.panic();
                }
            }
        }
    }
    private typeCheckTypeDefinition(typeDefinition: AST.ASTTypeDefinition) {
        for (let i = 0; i < typeDefinition.declarations.length; i++) {
            this.typeCheckDeclaration(typeDefinition.declarations[i]);
        }
    }
    private typeCheckTypeConstructStack: (AST.ASTTypeConstruction | AST.ASTDeclaration)[] = [];
    private typeCheckTypeConstruction(typeConstruct: AST.ASTTypeConstruction, scope: Scope) {
        let typeConsAssignments = typeConstruct.assignments;

        // TODO see issue on 12/30/20
        // TODO handle dot operator: TestModule.Stuff.Particle.{};
        let typeDefDecl = scope.searchVariableDeclaration(<AST.ASTName>typeConstruct.typeRef);
        if (!typeDefDecl || typeDefDecl.rvalue.nodeName !== AST.NodeType.TYPE_DEF) {
            this.err.atNode_PANIC(
                "Couldn't find type name '" + AST.exprToStr(typeConstruct.typeRef) + "'",
                typeConstruct
            );
        }
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
        this.typeCheckTypeConstructStack.push(typeDefDecl);
        this.typeCheckDeclaration(typeDefDecl);
        this.typeCheckTypeConstructStack = [];

        for (let i = 0; i < typeConsAssignments.length; i++) {
            let resolvedAssignmentType = this.typeOfExpression(typeConsAssignments[i].rvalue, scope);
            let assignmentVarName = (<AST.ASTName>typeConsAssignments[i].lvalue).refName;

            let typeDef = <AST.ASTTypeDefinition>typeDefDecl.rvalue;
            let typeVarDecl = undefined;
            for (let j = 0; j < typeDef.declarations.length; j++) {
                if ((<AST.ASTName>typeDef.declarations[j].lvalue).refName === assignmentVarName) {
                    typeVarDecl = typeDef.declarations[j];
                    break;
                }
            }
            if (typeVarDecl) {
                if (!this.isTypeEqual(resolvedAssignmentType, typeVarDecl.resolvedType, scope)) {
                    this.err
                        .atNode("Error in type construction", typeConstruct)
                        .atToken(
                            "Variable assignment with inferred type of " + AST.exprToStr(resolvedAssignmentType),
                            typeConsAssignments[i].locToken
                        )
                        .atNode_PANIC(
                            "...is incompatible with expected type of " + AST.exprToStr(typeVarDecl.resolvedType),
                            typeVarDecl
                        );
                }
            } else {
                this.err
                    .atNode("Error in type construction", typeConstruct)
                    .atToken_PANIC(
                        "Property name '" +
                            assignmentVarName +
                            "' does not exist in type '" +
                            AST.exprToStr(typeDefDecl.lvalue) +
                            "'",
                        typeConsAssignments[i].locToken
                    );
            }
        }
    }
}
