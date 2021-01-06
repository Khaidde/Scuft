import { ErrorHandler } from "./ErrorHandler";
import { Token, TokenType, tokenToStr } from "./Token";
import * as AST from "./Ast";

import Lexer from "./Lexer";
import Scope from "./Scope";

export default class Parser {
    private readonly lexer: Lexer;
    globalScope!: Scope;
    readonly err: ErrorHandler;
    constructor(lexer: Lexer, errHandler: ErrorHandler) {
        this.lexer = lexer;
        this.err = ErrorHandler.fromHandler("----Parser----\n", errHandler);

        this.initPrecedenceMap();
    }
    private throwAssertError(actualTkn: Token, expectedTkn: string, msg?: string): never {
        if (msg) {
            this.err.atAfterLastToken(
                "The parser expected " + expectedTkn + " but found a different token. " + msg,
                actualTkn
            );
        } else {
            this.err.atAfterLastToken("The parser expected " + expectedTkn + " but found a different token", actualTkn);
        }
        let actualTknStr = actualTkn.stringValue;
        if (actualTkn.type === TokenType.IDENTIFIER_TKN) {
            actualTknStr = "'" + actualTkn.stringValue + "'";
        }
        this.err.atWholeToken_PANIC(actualTknStr + " is not permitted here", actualTkn);
    }
    private assertIdentifier(expectedTkn: string, msg?: string) {
        let actualTkn = this.peekToken();
        if (actualTkn.type !== TokenType.IDENTIFIER_TKN) {
            this.throwAssertError(actualTkn, expectedTkn, msg);
        }
    }
    private assertToken(type: TokenType, msg?: string) {
        let actualTkn = this.peekToken();
        let expectedTkn = tokenToStr(type);
        if (actualTkn.type !== type) {
            this.throwAssertError(actualTkn, expectedTkn, msg);
        }
    }
    private tokenQueue: Token[] = [];
    private lookAheadToken(amount: number): Token {
        console.assert(amount > 0, "Must only look ahead by amount greater than 0. Instead got %s", amount);
        for (let i = this.tokenQueue.length; i < amount; i++) {
            this.tokenQueue.push(this.lexer.nextToken());
        }
        return this.tokenQueue[amount - 1];
    }
    private peekToken(): Token {
        return this.lookAheadToken(1);
    }
    private checkToken(type: TokenType): boolean {
        return this.peekToken().type === type;
    }
    private nextToken(): Token {
        if (this.tokenQueue.length > 0) {
            return this.tokenQueue.shift()!;
        } else {
            return this.lexer.nextToken();
        }
    }
    parseProgram(): AST.ASTProgram {
        this.globalScope = Scope.newGlobalScope();
        let program = new AST.ASTProgram(this.peekToken());
        program.operatorDefinitions = [];
        program.withModules = [];
        program.declarations = [];
        while (!this.checkToken(TokenType.END_TKN)) {
            if (this.checkToken(TokenType.WITH_TKN)) {
                program.withModules.push(this.parseWith());
            } else if (this.checkToken(TokenType.OPERATOR_TKN)) {
                program.operatorDefinitions.push(this.parseOperatorDefinition());
            } else {
                if (!this.checkToken(TokenType.IDENTIFIER_TKN)) {
                    this.err.atToken_PANIC(
                        "Statements are not allowed to exist freely in global scope",
                        this.peekToken()
                    );
                }
                let declaration = this.parseDeclaration(this.globalScope);
                program.declarations.push(declaration);
            }
        }
        return program;
    }
    private parseBlock(blockScope: Scope): AST.ASTBlock {
        let block = new AST.ASTBlock(this.peekToken());
        if (!this.checkToken(TokenType.LEFT_CURLY_TKN)) {
            let singleLineBlock = new AST.ASTBlock(this.peekToken());
            singleLineBlock.statements = [];
            singleLineBlock.statements.push(this.parseStatement(blockScope));
            return singleLineBlock;
        }
        this.nextToken(); // Consume {
        block.scope = blockScope;
        block.withModules = [];
        block.statements = [];
        while (!this.checkToken(TokenType.RIGHT_CURLY_TKN) && !this.checkToken(TokenType.END_TKN)) {
            if (this.checkToken(TokenType.WITH_TKN)) {
                block.withModules.push(this.parseWith());
            } else {
                let statement = this.parseStatement(blockScope);
                if (statement.nodeName === AST.NodeType.DECLARATION) {
                    let decl = <AST.ASTDeclaration>statement;
                    if (decl.rvalue) {
                        switch (decl.rvalue.nodeName) {
                            case AST.NodeType.MODULE:
                                this.err
                                    .atNode("Error in block", block)
                                    .atNode_PANIC("Module declaration is not allowed here", decl);
                            case AST.NodeType.TYPE_DEF:
                                this.err
                                    .atNode("Error in block", block)
                                    .atNode_PANIC("Type definition is not allowed here", decl);
                        }
                    }
                }
                block.statements.push(statement);
            }
        }
        if (this.checkToken(TokenType.END_TKN)) {
            this.err
                .atNode("Error in block", block)
                .atAfterLastToken_PANIC("Unterminated block at end of file. Expected closing }", this.peekToken());
        } else {
            this.nextToken();
        }
        return block;
    }
    private parseStatement(scope: Scope): AST.ASTStatement {
        switch (this.peekToken().type) {
            case TokenType.IF_TKN:
                return this.parseIf(scope);
            case TokenType.WHILE_TKN:
                return this.parseWhile(scope);
            case TokenType.FOR_TKN:
                return this.parseFor(scope);
            case TokenType.BREAK_TKN:
                let breakStatement = new AST.ASTBreak(this.nextToken());
                this.assertToken(TokenType.SEMI_COLON_TKN, "Semi-colons are required at the end of break statements");
                this.nextToken(); // Consume semi-colon;
                return breakStatement;
            case TokenType.CONTINUE_TKN:
                let continueStatement = new AST.ASTContinue(this.nextToken());
                this.assertToken(
                    TokenType.SEMI_COLON_TKN,
                    "Semi-colons are required at the end of continue statements"
                );
                this.nextToken(); // Consume semi-colon;
                return continueStatement;
            case TokenType.RETURN_TKN:
                let returnStatement = new AST.ASTReturn(this.nextToken());
                if (!this.checkToken(TokenType.SEMI_COLON_TKN)) {
                    returnStatement.returnValue = this.parseExpression();
                }
                this.assertToken(TokenType.SEMI_COLON_TKN, "Semi-colons are required at the end of return statements");
                this.nextToken(); // Consume semi-colon;
                return returnStatement;
            case TokenType.OPERATOR_TKN:
                this.err.atToken_PANIC(
                    "Operator definitions are only allowed in modules or in global scope",
                    this.peekToken()
                );
            default:
                let refName = this.parseExpression();
                if (
                    !this.checkToken(TokenType.COLON_TKN) &&
                    !this.checkToken(TokenType.MUTABLE_ASSIGNMENT_TKN) &&
                    !this.checkToken(TokenType.IMMUTABLE_ASSIGNMENT_TKN) &&
                    !this.checkToken(TokenType.CONST_ASSIGNMENT_TKN)
                ) {
                    this.assertToken(TokenType.SEMI_COLON_TKN, "This expression must end with a semi-colon");
                    this.nextToken(); // Consume semi-colon
                    return refName;
                } else {
                    return this.parseDeclarationFromName(refName, scope);
                }
        }
    }
    private parseWith(): AST.ASTExpression {
        this.nextToken(); //Consume with token
        this.assertIdentifier("a module name", "With keyword must be followed by a name reference to a module");
        let withModuleReference = this.parseExpression();
        this.assertToken(TokenType.SEMI_COLON_TKN, "Semi-colons are required at the end of with statements");
        this.nextToken(); //Consume semi-colon
        return withModuleReference;
    }
    private parseIf(scope: Scope): AST.ASTIf {
        let ifScope = Scope.newScopeFrom(scope);
        this.assertToken(TokenType.IF_TKN, "If statement must start with if keyword");
        let ifStatement = new AST.ASTIf(this.nextToken());
        ifStatement.condition = this.parseExpression();

        if (this.checkToken(TokenType.RIGHT_PARENS_TKN)) {
            this.err.atToken_PANIC("Unbalanced parenthesis. Expected {", this.peekToken());
        }
        ifStatement.consequence = this.parseBlock(ifScope);
        if (this.checkToken(TokenType.ELSE_TKN)) {
            this.nextToken(); //Consume else token
            if (this.checkToken(TokenType.IF_TKN)) {
                ifStatement.alternative = this.parseIf(scope);
            } else if (this.checkToken(TokenType.END_TKN)) {
                this.err
                    .atNode("Error in if statement", ifStatement)
                    .atAfterLastToken_PANIC(
                        "Unterminated code at end of file. Expected if keyword or {",
                        this.peekToken()
                    );
            } else {
                ifStatement.alternative = this.parseBlock(ifScope);
            }
        }
        return ifStatement;
    }
    private parseWhile(scope: Scope): AST.ASTWhile {
        this.assertToken(TokenType.WHILE_TKN, "While loop must start with while keyword");
        let whileStatement = new AST.ASTWhile(this.nextToken());
        whileStatement.condition = this.parseExpression();
        whileStatement.block = this.parseBlock(Scope.newScopeFrom(scope));
        return whileStatement;
    }
    private parseFor(scope: Scope): AST.ASTFor {
        let forScope = Scope.newScopeFrom(scope);
        this.assertToken(TokenType.FOR_TKN, "For loop must start with for keyword");
        let forStatement = new AST.ASTFor(this.nextToken());

        if (this.checkToken(TokenType.LEFT_PARENS_TKN)) {
            this.err.atToken_PANIC(
                "For loops don't require encapsulating parenthesis in this language",
                this.peekToken()
            );
        }
        switch (this.lookAheadToken(2).type) {
            case TokenType.LEFT_CURLY_TKN:
                this.assertIdentifier("a variable reference to an iterable");
                forStatement.iterableName = this.parseExpression();
                break;
            case TokenType.COMMA_TKN:
                let itemDec = new AST.ASTDeclaration(this.peekToken());
                forStatement.itemParamDec = itemDec;
                itemDec.inScope = forScope;
                itemDec.lvalue = new AST.ASTName(this.peekToken());
                if (!this.checkToken(TokenType.IDENTIFIER_TKN)) {
                    this.err.atToken_PANIC(
                        "Expected a new 'item' variable declaration but got an expression instead",
                        this.peekToken()
                    );
                }
                itemDec.lvalue.refName = this.nextToken().stringValue;
                forScope.addDeclaration(itemDec);
                this.nextToken(); //Consume comma

                let indexDec = new AST.ASTDeclaration(this.peekToken());
                forStatement.indexParamDec = indexDec;
                indexDec.inScope = forScope;
                indexDec.lvalue = new AST.ASTName(this.peekToken());
                indexDec.type = AST.newTypeLiteralNode(TokenType.NUM_TYPE_TKN, this.peekToken());
                if (!this.checkToken(TokenType.IDENTIFIER_TKN)) {
                    this.err.atToken_PANIC(
                        "Expected a new 'index' variable declaration but got an expression instead",
                        this.peekToken()
                    );
                }
                indexDec.lvalue.refName = this.nextToken().stringValue;
                if (itemDec.lvalue.refName === indexDec.lvalue.refName) {
                    this.err
                        .atNode("Duplicate variable declaration in for loop", itemDec)
                        .atNode_PANIC("Variable name of item and index variable must be different", indexDec);
                }
                forScope.addDeclaration(itemDec);

                this.assertToken(TokenType.IN_TKN);
                this.nextToken(); //Consume in token

                this.assertIdentifier("a variable reference to an iterable");
                forStatement.iterableName = this.parseExpression();
                break;
            case TokenType.IN_TKN:
                let inItemDec = new AST.ASTDeclaration(this.peekToken());
                forStatement.itemParamDec = inItemDec;
                inItemDec.inScope = forScope;
                inItemDec.lvalue = new AST.ASTName(this.peekToken());
                if (!this.checkToken(TokenType.IDENTIFIER_TKN)) {
                    this.err.atToken_PANIC(
                        "Expected a new 'item' variable declaration but got an expression instead",
                        this.peekToken()
                    );
                }
                inItemDec.lvalue.refName = this.nextToken().stringValue;
                forScope.addDeclaration(inItemDec);

                this.nextToken(); //Consume in

                if (this.checkToken(TokenType.HASH_RANGE_TKN)) {
                    this.nextToken(); //Consume #range token
                    let isLowerBoundExclusive = false;
                    if (this.checkToken(TokenType.LEFT_PARENS_TKN)) {
                        isLowerBoundExclusive = true;
                    } else if (!this.checkToken(TokenType.LEFT_SQUARE_TKN)) {
                        this.err.atToken_PANIC("Expected either a [ or a (", this.peekToken());
                    }
                    this.nextToken(); //consume ) or ]

                    forStatement.lowerBound = this.parseExpression();
                    if (isLowerBoundExclusive) {
                        let expression = new AST.ASTBinaryOperator(forStatement.lowerBound.locToken);
                        expression.lvalue = forStatement.lowerBound;
                        expression.operation = new Token(
                            "+",
                            forStatement.lowerBound.locToken.line,
                            forStatement.lowerBound.locToken.c,
                            TokenType.OP_ADD_TKN
                        );
                        expression.rvalue = AST.newLiteralNode(1, forStatement.lowerBound.locToken);
                        forStatement.lowerBound = expression;
                    }

                    if (this.checkToken(TokenType.RIGHT_SQUARE_TKN) || this.checkToken(TokenType.RIGHT_PARENS_TKN)) {
                        this.err.atToken_PANIC(
                            "For loop range requires two expressions separated by a comma",
                            this.peekToken()
                        );
                    }
                    this.assertToken(TokenType.COMMA_TKN, "Invalid declaration of a for loop range");
                    this.nextToken();

                    forStatement.upperBound = this.parseExpression();

                    if (this.checkToken(TokenType.RIGHT_PARENS_TKN)) {
                        let expression = new AST.ASTBinaryOperator(forStatement.upperBound.locToken);
                        expression.lvalue = forStatement.upperBound;
                        expression.operation = new Token(
                            "-",
                            forStatement.upperBound.locToken.line,
                            forStatement.upperBound.locToken.c,
                            TokenType.OP_SUBTR_TKN
                        );
                        expression.rvalue = AST.newLiteralNode(1, forStatement.upperBound.locToken);
                        forStatement.upperBound = expression;
                    } else if (!this.checkToken(TokenType.RIGHT_SQUARE_TKN)) {
                        this.err.atToken_PANIC("Expected either a ] or a )", this.peekToken());
                    }
                    this.nextToken(); //consume ) or ]
                    break;
                } else {
                    this.assertIdentifier(
                        "a variable reference to an iterable",
                        "A potential fix is to use #range after the in keyword to denote iteration over a range"
                    );
                    forStatement.iterableName = this.parseExpression();
                    break;
                }
            default:
                this.err.atToken_PANIC(
                    "Invalid for loop. Expected a single variable name but got an expression instead",
                    this.peekToken()
                );
        }
        forStatement.block = this.parseBlock(forScope);
        return forStatement;
    }
    private parseOperatorDefinition(): AST.ASTOperatorDefinition {
        let operatorDef = new AST.ASTOperatorDefinition(this.peekToken());
        this.assertToken(TokenType.OPERATOR_TKN, "Operator definition must start with operator keyword");
        this.nextToken(); //Consume operator token
        operatorDef.operatorName = this.nextToken();
        this.assertToken(
            TokenType.CONST_ASSIGNMENT_TKN,
            "Operator definition must be a constant assignment to a function overload"
        );
        this.nextToken(); //Consume =>
        operatorDef.functionOverload = this.parseFunction(this.globalScope);
        this.globalScope.addOperatorOverload(operatorDef.operatorName.type, operatorDef.functionOverload);
        return operatorDef;
    }
    private parseDeclaration(scope: Scope): AST.ASTDeclaration {
        return this.parseDeclarationFromName(this.parseExpression(), scope);
    }
    private parseDeclarationFromName(refName: AST.ASTExpression, scope: Scope): AST.ASTDeclaration {
        let decl = new AST.ASTDeclaration(refName.locToken);
        decl.inScope = scope;
        decl.lvalue = refName;
        if (this.checkToken(TokenType.COLON_TKN)) {
            this.nextToken(); //Consume :
            decl.type = this.parseExpression();
        }

        let accessTkn: Token;
        if (
            this.checkToken(TokenType.MUTABLE_ASSIGNMENT_TKN) ||
            this.checkToken(TokenType.IMMUTABLE_ASSIGNMENT_TKN) ||
            this.checkToken(TokenType.CONST_ASSIGNMENT_TKN)
        ) {
            accessTkn = this.nextToken();
            decl.accessAssignment = <AST.DeclarationAccessToken>accessTkn.type;
            switch (this.peekToken().type) {
                case TokenType.MODULE_TKN:
                    if (decl.accessAssignment === TokenType.CONST_ASSIGNMENT_TKN) {
                        decl.rvalue = this.parseModule(scope);
                        if (this.checkToken(TokenType.SEMI_COLON_TKN)) {
                            this.err
                                .atNode("Error in module declaration", refName)
                                .atToken_PANIC(
                                    "Semi-colons are not accepted directly after the end bracket of a module declaration",
                                    this.peekToken()
                                );
                        }
                        break;
                    } else {
                        this.err
                            .atNode("Error in module declaration", refName)
                            .atToken_PANIC(
                                "Module declarations must be constant: " + AST.exprToStr(refName) + " => module {...}",
                                accessTkn
                            );
                    }
                case TokenType.TYPE_TKN:
                    if (decl.accessAssignment === TokenType.CONST_ASSIGNMENT_TKN) {
                        decl.rvalue = this.parseTypeDefinition(Scope.newTypeDefScopeFrom(scope, decl));
                        if (this.checkToken(TokenType.SEMI_COLON_TKN)) {
                            this.err
                                .atNode("Error in type definition", refName)
                                .atToken_PANIC(
                                    "Semi-colons are not accepted directly after the end bracket of a type definition",
                                    this.peekToken()
                                );
                        }
                        break;
                    } else {
                        this.err
                            .atNode("Error in type definition", refName)
                            .atToken_PANIC(
                                "Type definitions must be constant: " + AST.exprToStr(refName) + " => type {...}",
                                accessTkn
                            );
                    }
                default:
                    //Distinguish function declaration from variable declaration
                    if (
                        (this.lookAheadToken(1).type === TokenType.LEFT_PARENS_TKN &&
                            this.lookAheadToken(2).type === TokenType.IDENTIFIER_TKN &&
                            (this.lookAheadToken(3).type === TokenType.COLON_TKN ||
                                this.lookAheadToken(3).type === TokenType.IMMUTABLE_ASSIGNMENT_TKN)) ||
                        (this.lookAheadToken(1).type === TokenType.LEFT_PARENS_TKN &&
                            this.lookAheadToken(2).type === TokenType.RIGHT_PARENS_TKN)
                    ) {
                        decl.rvalue = this.parseFunction(scope);
                        if (this.checkToken(TokenType.SEMI_COLON_TKN)) {
                            this.err
                                .atNode("Error in function declaration", refName)
                                .atToken_PANIC(
                                    "Semi-colons are not accepted after a function declaration",
                                    this.peekToken()
                                );
                        }
                    } else {
                        decl.rvalue = this.parseExpression();
                        if (this.checkToken(TokenType.RIGHT_PARENS_TKN)) {
                            this.err.atToken_PANIC("Unbalanced parenthesis. Expected a semi-colon", this.peekToken());
                        } else if (!this.checkToken(TokenType.SEMI_COLON_TKN)) {
                            let declNameType = decl.type ? "declaration" : "assignment";
                            this.err
                                .atNode("Error in " + declNameType, refName)
                                .atAfterLastToken_PANIC(
                                    "Expected a semi-colon at the end of the variable " +
                                        declNameType +
                                        " but got " +
                                        this.peekToken().stringValue +
                                        " instead",
                                    this.peekToken()
                                );
                        }
                        this.nextToken();
                    }
            }
        } else {
            this.assertToken(TokenType.SEMI_COLON_TKN, "Semi-colons are required at the end of variable declarations");
            this.nextToken(); // Consume semi-colon
        }

        console.assert(
            decl.type || decl.rvalue,
            "This should be evaluted as an expression statement, not a declaration"
        );
        if (refName.nodeName === AST.NodeType.NAME) {
            let isFunc = decl.rvalue && decl.rvalue.nodeName === AST.NodeType.FUNCTION;
            if (isFunc || !scope.table.has((<AST.ASTName>refName).refName)) {
                if (isFunc && decl.accessAssignment === TokenType.MUTABLE_ASSIGNMENT_TKN) {
                    this.err
                        .atNode("Function can't be assigned using mutable assignment", decl)
                        .addNote_PANIC("Functions can only be assigned to a variable using = or =>");
                }
                scope.addDeclaration(decl);
                return decl;
            }
        }
        decl.isAssignment = true;
        return decl;
    }
    private parseModule(scope: Scope): AST.ASTModule {
        let moduleScope = Scope.newModuleScopeFrom(scope);
        this.assertToken(TokenType.MODULE_TKN, "Module must start with module keyword");
        let module = new AST.ASTModule(this.nextToken());
        module.withModules = [];
        module.declarations = [];
        this.assertToken(TokenType.LEFT_CURLY_TKN);
        this.nextToken(); //Consume left curly
        while (!this.checkToken(TokenType.RIGHT_CURLY_TKN) && !this.checkToken(TokenType.END_TKN)) {
            if (this.checkToken(TokenType.WITH_TKN)) {
                module.withModules.push(this.parseWith());
            } else if (this.checkToken(TokenType.OPERATOR_TKN)) {
                this.err
                    .atNode("Error in module", module)
                    .atToken_PANIC("Modules are not yet allowed to define operator overloads", this.peekToken());
            } else {
                module.declarations.push(this.parseDeclaration(moduleScope));
            }
        }
        if (this.checkToken(TokenType.END_TKN)) {
            this.err
                .atNode("Error in module", module)
                .atAfterLastToken_PANIC("Unterminated code at end of file. Expected closing {", this.peekToken());
        } else {
            this.nextToken();
        }
        return module;
    }
    private parseTypeDefinition(typeDefScope: Scope): AST.ASTTypeDefinition {
        this.assertToken(TokenType.TYPE_TKN, "Type definition must start with type keyword");
        let typeDef = new AST.ASTTypeDefinition(this.nextToken());
        typeDef.withModules = [];
        typeDef.declarations = [];
        this.assertToken(TokenType.LEFT_CURLY_TKN, "Type definition must be enclosed in curly brackets");
        this.nextToken(); //Consume left curly
        while (!this.checkToken(TokenType.RIGHT_CURLY_TKN) && !this.checkToken(TokenType.END_TKN)) {
            if (this.checkToken(TokenType.WITH_TKN)) {
                typeDef.withModules.push(this.parseWith());
            } else if (this.checkToken(TokenType.OPERATOR_TKN)) {
                this.err
                    .atNode("Error in type definition", typeDef)
                    .atToken_PANIC("Type definitions are not allowed to define operator overloads", this.peekToken());
            } else {
                typeDef.declarations.push(this.parseDeclaration(typeDefScope));
            }
        }
        if (this.checkToken(TokenType.END_TKN)) {
            this.err
                .atNode("Error in type definition", typeDef)
                .atAfterLastToken_PANIC("Unterminated code at end of file. Expected closing {", this.peekToken());
        } else {
            this.nextToken();
        }
        return typeDef;
    }
    private parseFunction(scope: Scope): AST.ASTFunction {
        let functionScope = Scope.newScopeFrom(scope);
        let func = new AST.ASTFunction(this.peekToken());

        func.paramDeclaration = [];

        this.assertToken(TokenType.LEFT_PARENS_TKN, "Function declaration must specify parameters or use ()");
        this.nextToken(); //Consume left parenthesis

        while (!this.checkToken(TokenType.RIGHT_PARENS_TKN) && !this.checkToken(TokenType.END_TKN)) {
            let paramDec = new AST.ASTDeclaration(this.peekToken());
            paramDec.inScope = functionScope;
            this.assertIdentifier("a parameter name", "Function parameters must declare variable names");
            paramDec.lvalue = new AST.ASTName(this.peekToken());
            paramDec.lvalue.refName = this.nextToken().stringValue;
            let otherDec = functionScope.getVariable(paramDec.lvalue);
            if (otherDec) {
                this.err
                    .atNode("Found duplicate name for parameter declaration", paramDec)
                    .atNode_PANIC("Other duplicate parameter here", otherDec);
            } else {
                functionScope.addDeclaration(paramDec);
            }
            if (this.checkToken(TokenType.COLON_TKN)) {
                this.nextToken(); //Consume colon
                paramDec.type = this.parseExpression();
            }
            if (this.checkToken(TokenType.IMMUTABLE_ASSIGNMENT_TKN)) {
                this.nextToken(); //Consume =
                paramDec.rvalue = this.parseExpression();
            }
            if (!paramDec.type && !paramDec.rvalue) {
                this.err
                    .atNode("Error in function definition", paramDec)
                    .atToken_PANIC(
                        this.peekToken().stringValue +
                            " is not allowed here. Function parameter must either specify a type using : or assign a default value using =",
                        this.peekToken()
                    );
            }
            func.paramDeclaration.push(paramDec);
            if (this.checkToken(TokenType.COMMA_TKN)) {
                let expectedTkn = this.lookAheadToken(2);
                if (expectedTkn.type === TokenType.RIGHT_PARENS_TKN) {
                    // a = (b: num,  )
                    //             ^
                    this.err.atAfterLastToken_PANIC(
                        "Expected another parameter definition after the comma but got nothing",
                        expectedTkn
                    );
                } else {
                    this.nextToken();
                }
            } else if (!this.checkToken(TokenType.RIGHT_PARENS_TKN)) {
                // a = (a:num  b:num)
                //           ^
                this.err.atAfterLastToken_PANIC(
                    "Invalid function parameter definition. Expected either a comma or )",
                    this.peekToken()
                );
            }
        }
        if (this.checkToken(TokenType.END_TKN)) {
            //a = (a:num,     [End of File]
            //           ^
            this.err.atAfterLastToken_PANIC("Unterminated parameter declaration at end of file", this.peekToken());
        }
        this.nextToken();

        if (this.checkToken(TokenType.ARROW_TKN)) {
            this.nextToken();
            func.returnType = this.parseExpression();
        }
        func.block = this.parseBlock(functionScope);

        return func;
    }

    private precedence: Map<TokenType, number> = new Map();
    readonly LOWEST_PRECEDENCE: number = 0;
    readonly HIGHEST_PRECEDENCE: number = 100;
    private setPrecedence(precedence: number, ...types: TokenType[]) {
        for (let i = 0; i < types.length; i++) {
            this.precedence.set(types[i], precedence);
        }
    }
    private initPrecedenceMap() {
        // Boolean Operators
        this.setPrecedence(1, TokenType.COND_OR_TKN);
        this.setPrecedence(2, TokenType.COND_XOR_TKN);
        this.setPrecedence(3, TokenType.COND_AND_TKN);

        this.setPrecedence(4, TokenType.BIN_OR_TKN);
        this.setPrecedence(5, TokenType.BIN_XOR_TKN);
        this.setPrecedence(6, TokenType.BIN_AND_TKN);

        // Booleans can be compared, so (==) has to be lower precedence.
        this.setPrecedence(7, TokenType.COND_EQUALS_TKN, TokenType.COND_NOT_EQUALS_TKN);
        this.setPrecedence(
            8,
            TokenType.COND_GREATER_THAN_EQUAL_TKN,
            TokenType.COND_LESS_THAN_EQUAL_TKN,
            TokenType.COND_GREATER_THAN_TKN,
            TokenType.COND_LESS_THAN_TKN
        );

        this.setPrecedence(
            9,
            TokenType.BIN_SHIFT_LEFT_TKN,
            TokenType.BIN_SHIFT_RIGHT_TKN,
            TokenType.BIN_SHIFT_ARITHMETIC_RIGHT_TKN
        );

        // Numeric Operations
        this.setPrecedence(10, TokenType.OP_ADD_TKN, TokenType.OP_SUBTR_TKN);
        this.setPrecedence(11, TokenType.OP_MULT_TKN, TokenType.OP_DIVIDE_TKN, TokenType.OP_MODULUS_TKN);
        this.setPrecedence(12, TokenType.OP_CARROT_TKN);

        this.setPrecedence(13, TokenType.DOT_TKN, TokenType.LEFT_PARENS_TKN);
    }
    private parseExpression(): AST.ASTExpression {
        return this.recurseExpression(this.LOWEST_PRECEDENCE, 0, this.peekToken());
    }
    private recurseExpression(precedence: number, depthCount: number, sourceToken: Token): AST.ASTExpression {
        let expression!: AST.ASTExpression;
        while (!this.checkToken(TokenType.END_TKN)) {
            let tkn = this.peekToken();
            switch (tkn.type) {
                case TokenType.MODULE_TKN:
                    this.err
                        .atToken("Error in expression starting with", sourceToken)
                        .atToken_PANIC(
                            "Modules should strictly be assigned to a variable in the form [moduleName] => module {...}",
                            tkn
                        );
                case TokenType.TYPE_TKN:
                    if (this.lookAheadToken(2).type === TokenType.LEFT_CURLY_TKN) {
                        this.err
                            .atToken("Error in expression starting with", sourceToken)
                            .atToken_PANIC(
                                "Modules should strictly be assigned to a variable in the form [typeName] => type {...}",
                                tkn
                            );
                    }
            }
            if (expression) {
                switch (tkn.type) {
                    case TokenType.MUT_CAST_TKN:
                    case TokenType.CONST_CAST_TKN:
                    case TokenType.NUM_TYPE_TKN:
                    case TokenType.STRING_TYPE_TKN:
                    case TokenType.BOOL_TYPE_TKN:
                    case TokenType.VOID_TYPE_TKN:
                    case TokenType.TYPE_TKN:
                    case TokenType.NUMERIC_LITERAL_TKN:
                    case TokenType.STRING_LITERAL_TKN:
                    case TokenType.COND_TRUE_TKN:
                    case TokenType.COND_FALSE_TKN:
                    case TokenType.COND_NOT_TKN:
                    case TokenType.BIN_NOT_TKN:
                    case TokenType.BACKSLASH_TKN:
                    case TokenType.ELLIPSIS_TKN:
                    case TokenType.IDENTIFIER_TKN:
                        if (depthCount !== 0) {
                            this.err
                                .insert("First potential error:\n")
                                .atToken(
                                    "Operand token (ex: 3, true, etc.) or prefix operator (ex: !, ~, etc.) can only appear after an operator (ex: +, *, etc.)",
                                    tkn
                                )
                                .insert("Second potential error:\n")
                                .atAfterLastToken_PANIC(
                                    "Unbalanced parenthesis. Expected " + depthCount + " more )",
                                    tkn
                                );
                        }
                        return expression;
                    case TokenType.RIGHT_PARENS_TKN:
                        return expression;
                    case TokenType.DOT_TKN:
                        if (this.lookAheadToken(2).type === TokenType.LEFT_CURLY_TKN) {
                            expression = this.parseTypeConstruction(expression);
                            break;
                        }
                    default:
                        let curPrecedence = this.precedence.get(tkn.type);
                        if (curPrecedence !== undefined) {
                            if (curPrecedence > precedence) {
                                if (this.checkToken(TokenType.DOT_TKN)) {
                                    let dotOp = new AST.ASTDotOperator(expression.locToken);
                                    dotOp.rootValue = expression;
                                    this.nextToken(); //Consume the dot token
                                    dotOp.memberValue = this.recurseExpression(curPrecedence, depthCount, sourceToken);
                                    expression = dotOp;
                                } else if (this.checkToken(TokenType.LEFT_PARENS_TKN)) {
                                    expression = this.parseCall(expression, sourceToken);
                                } else {
                                    if (this.checkToken(TokenType.BIN_SHIFT_LEFT_TKN)) {
                                        if (this.lookAheadToken(2).type === TokenType.COND_LESS_THAN_TKN) {
                                            this.err.atToken_PANIC(
                                                "Arithmetic left shift is equivalent to logical left shift. Use << instead of <<<",
                                                this.peekToken()
                                            );
                                        }
                                    }
                                    let binOp = new AST.ASTBinaryOperator(expression.locToken);
                                    binOp.lvalue = expression;
                                    binOp.operation = this.nextToken();
                                    binOp.rvalue = this.recurseExpression(curPrecedence, depthCount, sourceToken);
                                    expression = binOp;
                                }
                            } else {
                                return expression;
                            }
                        } else {
                            if (this.checkToken(TokenType.UNKNOWN_TKN)) {
                                this.err.atToken_PANIC(
                                    "Expression contains unknown operator " + this.peekToken().stringValue,
                                    this.peekToken()
                                ); // a = 3 # 4;
                            } else if (depthCount !== 0) {
                                this.err
                                    .atToken("Error in expression starting with", sourceToken)
                                    .atToken_PANIC("Unbalanced parenthesis. Expected " + depthCount + " more )", tkn); //b = (((((0 + 5)
                            } else {
                                return expression;
                            }
                        }
                }
            } else {
                switch (tkn.type) {
                    case TokenType.MUT_CAST_TKN:
                    case TokenType.CONST_CAST_TKN:
                        expression = new AST.ASTAccessCast(tkn);
                        expression.accessType = <AST.AccessCastToken>this.nextToken().type;
                        expression.castedType = this.recurseExpression(
                            this.HIGHEST_PRECEDENCE,
                            depthCount,
                            sourceToken
                        );
                        break;
                    case TokenType.NUM_TYPE_TKN:
                    case TokenType.STRING_TYPE_TKN:
                    case TokenType.BOOL_TYPE_TKN:
                    case TokenType.VOID_TYPE_TKN:
                    case TokenType.TYPE_TKN:
                        expression = new AST.ASTTypeLiteral(tkn);
                        expression.type = <AST.TypeLiteralToken>this.nextToken().type;
                        break;
                    case TokenType.NUMERIC_LITERAL_TKN:
                    case TokenType.STRING_LITERAL_TKN:
                    case TokenType.COND_TRUE_TKN:
                    case TokenType.COND_FALSE_TKN:
                        expression = new AST.ASTLiteral(tkn);
                        expression.value = this.nextToken();
                        break;
                    case TokenType.COND_NOT_TKN:
                    case TokenType.BIN_NOT_TKN:
                        expression = new AST.ASTUnaryOperator(tkn);
                        expression.operation = this.nextToken();
                        expression.value = this.recurseExpression(precedence, depthCount, sourceToken);
                        break;
                    case TokenType.BACKSLASH_TKN:
                        if (this.lookAheadToken(2).type === TokenType.LEFT_PARENS_TKN) {
                            expression = this.parseFunctionType();
                        } else {
                            expression = this.parseLambda();
                        }
                        break;
                    case TokenType.ELLIPSIS_TKN:
                        expression = new AST.ASTName(tkn);
                        expression.refName = tkn.stringValue;
                        this.nextToken();
                        if (this.checkToken(TokenType.DOT_TKN) || this.checkToken(TokenType.ELLIPSIS_TKN)) {
                            this.err.atToken_PANIC(
                                "Too many dots in a row. Note that an ellipsis has three dots: ...",
                                this.peekToken()
                            );
                        }
                        break;
                    case TokenType.IDENTIFIER_TKN:
                        expression = new AST.ASTName(this.nextToken());
                        expression.refName = expression.locToken.stringValue;
                        break;
                    case TokenType.LEFT_PARENS_TKN:
                        //This is an open parenthesis
                        this.nextToken(); //Consume left parenthesis
                        expression = this.recurseExpression(this.LOWEST_PRECEDENCE, depthCount + 1, sourceToken);
                        this.nextToken(); //Consume right parenthesis
                        break;
                    case TokenType.DOT_TKN:
                        this.err
                            .atToken("Error in reference starting with", sourceToken)
                            .atToken_PANIC(
                                "Can't start an expression with a dot or have two dots in a row",
                                this.peekToken()
                            );
                    case TokenType.OP_SUBTR_TKN:
                        // This is negation
                        expression = new AST.ASTBinaryOperator(tkn);
                        expression.lvalue = AST.newLiteralNode(0, tkn);
                        expression.operation = this.nextToken();
                        expression.rvalue = this.recurseExpression(this.HIGHEST_PRECEDENCE, depthCount, sourceToken);
                        break;
                    default:
                        this.err.atAfterLastToken_PANIC("Expected an expression but got nothing", tkn); //a = 3 + ;
                }
            }
        }
        this.err
            .atToken("Error in expression starting with", sourceToken)
            .atAfterLastToken_PANIC(
                "Unterminated code at end of file. Expected an operator, operand, or terminating token like semi-colon",
                this.peekToken()
            );
        return expression;
    }
    private parseCall(prevExpression: AST.ASTExpression, sourceToken: Token): AST.ASTCall {
        let call = new AST.ASTCall(prevExpression.locToken);
        call.functionNameRef = prevExpression;

        call.givenParams = [];

        this.nextToken(); //Consume left parenthesis
        while (!this.checkToken(TokenType.RIGHT_PARENS_TKN)) {
            if (this.checkToken(TokenType.COMMA_TKN)) {
                let expectedTkn = this.lookAheadToken(2);
                if (expectedTkn.type === TokenType.RIGHT_PARENS_TKN || expectedTkn.type === TokenType.END_TKN) {
                    // a(2, 32,   )
                    //         ^
                    this.err.atAfterLastToken_PANIC(
                        "Expected another expression after the comma but got nothing",
                        expectedTkn
                    );
                } else {
                    this.nextToken();
                }
            } else {
                call.givenParams.push(this.recurseExpression(this.LOWEST_PRECEDENCE, 0, sourceToken));
            }
        }
        this.nextToken(); // Consume right parenthesis
        return call;
    }

    private parseTypeConstruction(prevExpression: AST.ASTExpression): AST.ASTTypeConstruction {
        let typeConstruct = new AST.ASTTypeConstruction(prevExpression.locToken);
        typeConstruct.typeRef = prevExpression;
        this.assertToken(TokenType.DOT_TKN, "Dot '.' is required after the type name in a type construction");
        this.nextToken(); //Consume dot
        this.assertToken(TokenType.LEFT_CURLY_TKN, "Type construction must be enclosed in curly brackets");
        this.nextToken(); //Consume left curly
        typeConstruct.assignments = [];
        while (!this.checkToken(TokenType.RIGHT_CURLY_TKN)) {
            if (!this.checkToken(TokenType.IDENTIFIER_TKN)) {
                this.err
                    .atNode("Error in type constructor", typeConstruct)
                    .atToken_PANIC("Type construction assignments need to be single variable names", this.peekToken());
            }
            let lvalue = new AST.ASTName(this.peekToken());
            lvalue.refName = this.nextToken().stringValue;
            for (let i = 0; i < typeConstruct.assignments.length; i++) {
                if ((<AST.ASTName>typeConstruct.assignments[i].lvalue).refName === lvalue.refName) {
                    this.err
                        .atToken("Repeated assignment in type construction", typeConstruct.assignments[i].locToken)
                        .atNode_PANIC("Other repeated assignment found here", lvalue);
                }
            }
            if (this.checkToken(TokenType.IMMUTABLE_ASSIGNMENT_TKN)) {
                this.err
                    .atNode("Error in type constructor", typeConstruct)
                    .atToken_PANIC("Assignment in a type constructor must use <- instead of =", this.peekToken());
            } else if (!this.checkToken(TokenType.REVERSE_ARROW_TKN)) {
                this.err
                    .atToken("Assignment in a type constructor must assign a value using <-", this.peekToken())
                    .addNote_PANIC(
                        "Type consteuction assignments need to be in the form [singleVariableName] <- [assignedValue]"
                    );
            }
            this.nextToken(); //Consume <-
            let assignment = {
                locToken: lvalue.locToken,
                lvalue: lvalue,
                rvalue: this.parseExpression(),
            };
            typeConstruct.assignments.push(assignment);
            if (this.checkToken(TokenType.COMMA_TKN)) {
                let expectedTkn = this.lookAheadToken(2);
                if (expectedTkn.type === TokenType.RIGHT_CURLY_TKN) {
                    // particle = Particle { a = 3,   };
                    //                             ^
                    this.err.atAfterLastToken_PANIC(
                        "Expected another assignment after the comma but got nothing",
                        expectedTkn
                    );
                } else {
                    this.nextToken();
                }
            } else if (!this.checkToken(TokenType.RIGHT_CURLY_TKN)) {
                // particle = Particle { a = 3;    b = 4 };
                //                            ^
                this.err.atAfterLastToken_PANIC(
                    "Invalid type construction. Expected either a comma or }",
                    this.peekToken()
                );
            }
        }
        this.nextToken(); //Consume right curly token
        return typeConstruct;
    }
    private parseFunctionType(): AST.ASTFunctionType {
        this.assertToken(TokenType.BACKSLASH_TKN, "Function type must start with a backslash");
        this.nextToken();

        let type = new AST.ASTFunctionType(this.peekToken());
        this.nextToken();
        type.inputType = [];
        while (!this.checkToken(TokenType.RIGHT_PARENS_TKN) && !this.checkToken(TokenType.END_TKN)) {
            type.inputType.push(this.parseExpression());
            if (this.checkToken(TokenType.COMMA_TKN)) {
                this.nextToken();
            } else if (!this.checkToken(TokenType.RIGHT_PARENS_TKN)) {
                // a: (num; )
                //        ^
                this.err.atAfterLastToken_PANIC(
                    "Invalid type definition. Expected either a comma or )",
                    this.peekToken()
                );
            }
        }
        if (this.checkToken(TokenType.END_TKN)) {
            // a : (num, num,
            this.err
                .atNode("Error in declaration type specifier", type)
                .atAfterLastToken_PANIC("Unterminated code at end of file. Expected another type", this.peekToken());
        } else {
            this.nextToken(); //Consume right parenthesis
        }

        this.assertToken(TokenType.ARROW_TKN, "Function type declarations must declare a return type.");
        this.nextToken(); //Consume -> function
        type.outType = this.parseExpression();
        return type;
    }
    private parseLambda(): AST.ASTLambda {
        this.assertToken(TokenType.BACKSLASH_TKN, "Lambda expression must start with \\");
        let lambda = new AST.ASTLambda(this.nextToken());

        lambda.parameters = [];
        while (!this.checkToken(TokenType.END_TKN)) {
            let param = new AST.ASTName(this.peekToken());
            this.assertIdentifier(
                "a name parameter to a lambda expression",
                "Lambda parameters must be variable names"
            );
            param.refName = this.nextToken().stringValue;
            lambda.parameters.push(param);
            if (this.checkToken(TokenType.COMMA_TKN)) {
                this.nextToken(); //Consume the comma
            } else {
                break;
            }
        }
        if (this.checkToken(TokenType.END_TKN)) {
            this.err
                .atNode("Error in lambda expression", lambda)
                .atAfterLastToken_PANIC(
                    "Unterminated code at end of file. Expected another parameter",
                    this.peekToken()
                );
        }
        this.assertToken(TokenType.CONST_ASSIGNMENT_TKN, "Lambdas require => after the parameters");
        this.nextToken(); // Consume =>
        if (this.checkToken(TokenType.RETURN_TKN)) {
            this.err.atToken_PANIC("Return keyword is not accepted in a lambda definition", this.peekToken());
        }
        lambda.expression = this.parseExpression();

        return lambda;
    }
}
