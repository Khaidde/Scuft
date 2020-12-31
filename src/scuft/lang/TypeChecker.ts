import * as AST from "./Ast";
import { ErrorHandler } from "./ErrorHandler";
import { Scope } from "./Scope";
import { Token, TokenType } from "./Token";

export default class TypeChecker {
    private readonly globalScope!: Scope;
    private readonly err: ErrorHandler;
    constructor(globalScope: Scope, errHandler: ErrorHandler) {
        this.globalScope = globalScope;
        this.err = ErrorHandler.fromHandler("----TypeChecker----\n", errHandler);
    }
    typeInferProgram(astPrgm: AST.ASTProgram) {
        for (let i = 0; i < astPrgm.statements.length; i++) {
            let statement = astPrgm.statements[i];
            switch (statement.nodeName) {
                case AST.NodeType.DECLARATION:
                    this.typeInferDeclaration(this.globalScope, <AST.ASTDeclaration>statement);
                    break;
            }
        }
    }
    private typeInferDeclaration(scope: Scope, astDeclare: AST.ASTDeclaration) {
        if (astDeclare.rvalue) {
            let expressionType = this.getTypeOfExpression(scope, <AST.ASTExpression>astDeclare.rvalue);
            if (!astDeclare.type) {
                astDeclare.type = expressionType;
            } else if (!AST.isExpressionEqual(expressionType, astDeclare.type)) {
                throw "mismatched type";
            }
        }
        // point.a: string;
        // a: num;
        let otherDec = scope.getDeclarationFromRef(astDeclare.lvalue);
        if (otherDec) {
            throw "duplicate!!!";
        } else {
            return scope.table.set((<AST.ASTName>astDeclare.lvalue).refName, astDeclare);
        }
    }
    private typeInferSingleVarDeclaration(scope: Scope, astDeclare: AST.ASTSingleVarDeclaration) {
        if (astDeclare.rvalue) {
            let expressionType = this.getTypeOfExpression(scope, <AST.ASTExpression>astDeclare.rvalue);
            // a: string = 3;
            if (!astDeclare.type) {
                astDeclare.type = expressionType;
            } else if (!AST.isExpressionEqual(expressionType, astDeclare.type)) {
                throw "mismatched type";
            }
        }
        if (scope.table.has(astDeclare.lvalue.refName)) {
            throw "duplicate type definition variable in the same scope";
        }
        return scope.table.set(astDeclare.lvalue.refName, astDeclare);
    }
    private makeTypeLiteral(stringValue: string, locToken: Token, tokenType: TokenType): AST.ASTTypeLiteral {
        let typeLiteral = new AST.ASTTypeLiteral(locToken);
        typeLiteral.type = new Token(stringValue, locToken.line, locToken.c, tokenType);
        return typeLiteral;
    }
    // a = (3 + 5);
    private getTypeOfExpression(scope: Scope, astExpression: AST.ASTExpression): AST.ASTType {
        switch (astExpression.nodeName) {
            case AST.NodeType.MODULE:
                return this.makeTypeLiteral("module", astExpression.locToken, TokenType.MODULE_TKN);
            case AST.NodeType.TYPE_DEF:
                let astTypeDef = <AST.ASTTypeDefinition>astExpression;
                let typeScope = Scope.newScopeFrom(scope);
                for (let i = 0; i < astTypeDef.declarations.length; i++) {
                    this.typeInferSingleVarDeclaration(typeScope, astTypeDef.declarations[i]);
                }
            case AST.NodeType.TYPE_LITERAL:
                return this.makeTypeLiteral("type", astExpression.locToken, TokenType.TYPE_TKN);
            case AST.NodeType.LITERAL:
                let astLiteral = <AST.ASTLiteral>astExpression;
                switch (astLiteral.value.type) {
                    case TokenType.NUMERIC_LITERAL_TKN:
                        return this.makeTypeLiteral("num", astExpression.locToken, TokenType.NUM_TYPE_TKN);
                    case TokenType.STRING_LITERAL_TKN:
                        return this.makeTypeLiteral("string", astExpression.locToken, TokenType.STRING_TYPE_TKN);
                    case TokenType.COND_TRUE_TKN:
                    case TokenType.COND_FALSE_TKN:
                        return this.makeTypeLiteral("bool", astExpression.locToken, TokenType.BOOL_TYPE_TKN);
                    default:
                        throw "ASTLiteral does not contain a literal token";
                }
            case AST.NodeType.NAME:
                let astName = <AST.ASTName>astExpression;
                let nameDeclaration = scope.getDeclarationFromRef(astName);
                if (nameDeclaration) {
                    return nameDeclaration.type;
                } else {
                    // TODO decide whether to do recursion to type infer or to create an event system
                    throw "type of name has not yet been type inferred. Implement in future";
                }
            case AST.NodeType.TYPE_CONSTRUCT:
                let astTypeCons = <AST.ASTTypeConstruction>astExpression;

                // TODO see issues, 12/30/20. Check whether typeRef is a reference before casting
                let typeDef = scope.getDeclarationFromRef(<AST.ASTReference>astTypeCons.typeRef);
                if (!typeDef) throw "type doesn't exist";
                if (typeDef.rvalue.nodeName !== AST.NodeType.TYPE_DEF) throw "declaration is not a type definition";
                let typeAssigns = (<AST.ASTTypeDefinition>typeDef.rvalue).declarations;
                let assignedIndices: number[] = [];
                for (let i = 0; i < astTypeCons.assignments.length; i++) {
                    let name = astTypeCons.assignments[i].lvalue;
                    let assignIndex: number = -1;
                    for (let j = 0; j < typeAssigns.length; j++) {
                        if (typeAssigns[j].lvalue.refName === name.refName) {
                            for (let k = 0; k < assignedIndices.length; k++) {
                                if (assignedIndices[k] == j) {
                                    throw "Repeated type construction assignment";
                                }
                            }
                            assignedIndices.push(j);
                            assignIndex = j;
                            break;
                        }
                    }
                    if (assignIndex === -1) {
                        throw "parameter doesn't exist";
                    }

                    if (astTypeCons.assignments[i].rvalue.nodeName === AST.NodeType.FUNCTION) {
                        if (typeAssigns[assignIndex].rvalue.nodeName !== AST.NodeType.FUNCTION) {
                            throw "incorrect type2";
                        }
                        //TODO
                    } else {
                        if (typeAssigns[assignIndex].rvalue) {
                            if (typeAssigns[assignIndex].rvalue.nodeName === AST.NodeType.FUNCTION) {
                                throw "incorrect type1";
                            }
                        }
                        let assignType = this.getTypeOfExpression(scope, astTypeCons.assignments[i].rvalue);
                        let typeDefType;
                        if (typeAssigns[assignIndex].type) {
                            typeDefType = typeAssigns[assignIndex].type;
                        } else {
                            typeDefType = this.getTypeOfExpression(scope, typeAssigns[assignIndex].rvalue);
                        }
                        if (!AST.isExpressionEqual(assignType, typeDefType)) {
                            throw "types not the same";
                        }
                    }
                }
                // still have to include dot case
                return <AST.ASTName>astTypeCons.typeRef; // assuming not like test.Particle

            // a = (test . Particle).{x <- 3, x <- 7};
            default:
                throw "You done fucked up";
        }
    }
}
