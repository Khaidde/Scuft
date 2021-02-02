import { ErrorHandler } from "./ErrorHandler";
import { Token, TokenType, tokenToStr } from "./Token";
import * as AST from "./Ast";
import { Scope, ScopeType } from "./Scope";

import Lexer from "./Lexer";

export default class Parser {
    private readonly lexer: Lexer;
    readonly err: ErrorHandler;
    constructor(lexer: Lexer, errHandler: ErrorHandler) {
        this.lexer = lexer;
        this.err = ErrorHandler.fromHandler("----Parser----\n", errHandler);

        this.initPrecedenceMap();
    }
    private throwAssertError(actualTkn: Token, expectedTkn: string, msg?: string): never {
        if (msg) {
            this.err.atAfterLastToken("Expected " + expectedTkn + " but found a different token. " + msg, actualTkn);
        } else {
            this.err.atAfterLastToken("Expected " + expectedTkn + " but found a different token", actualTkn);
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
    private checkAheadToken(amount: number, type: TokenType): boolean {
        return this.lookAheadToken(amount).type === type;
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
        let program = new AST.ASTProgram(this.peekToken());
        program.scope = Scope.newGlobalScope();
        program.declarations = [];
        while (!this.checkToken(TokenType.END_TKN)) {
            if (!this.checkToken(TokenType.IDENTIFIER_TKN)) {
                this.err.atToken_PANIC("Statements are not allowed to exist freely in global scope", this.peekToken());
            }
            let declaration = this.parseDeclaration(program.scope);
            program.declarations.push(declaration);
        }
        return program;
    }
    private parseBlock(blockScope: Scope): AST.ASTBlock {
        let block = new AST.ASTBlock(this.peekToken());
        if (!this.checkToken(TokenType.LEFT_CURLY_TKN)) {
            let singleLineBlock = new AST.ASTBlock(this.peekToken());
            singleLineBlock.scope = blockScope;
            singleLineBlock.statements = [];
            singleLineBlock.statements.push(this.getStatement(singleLineBlock));
            singleLineBlock.endToken = singleLineBlock.statements[0].endToken;
            return singleLineBlock;
        }
        this.nextToken(); // Consume {
        block.scope = blockScope;
        block.statements = [];
        while (!this.checkToken(TokenType.RIGHT_CURLY_TKN) && !this.checkToken(TokenType.END_TKN)) {
            if (block.hasJump) {
                let lastStmt = block.statements[block.statements.length - 1];
                if (lastStmt.nodeName === AST.NodeType.IF) {
                    this.err
                        .atNode("Error after if statement starting here", lastStmt)
                        .atToken("End of block reached after the }", lastStmt.endToken)
                        .atWholeToken_PANIC(
                            "If statement has no leaks. The following statement is unreachable and not allowed by this language",
                            this.peekToken()
                        );
                } else {
                    this.err
                        .atWholeNode("End of block reached here", lastStmt)
                        .atWholeToken_PANIC(
                            "Statements following a " +
                                block.statements[block.statements.length - 1].locToken.stringValue +
                                " are unreachable and not allowed by this language",
                            this.peekToken()
                        );
                }
            }
            let statement = this.getStatement(block);
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
        if (this.checkToken(TokenType.END_TKN)) {
            this.err
                .atNode("Error in block", block)
                .atAfterLastToken_PANIC("Unterminated block at end of file. Expected closing }", this.peekToken());
        } else {
            block.endToken = this.nextToken();
        }
        return block;
    }
    private getStatement(block: AST.ASTBlock): AST.ASTStatement {
        let blockScope = block.scope;
        switch (this.peekToken().type) {
            case TokenType.BREAK_TKN:
                let breakStatement = new AST.ASTBreak(this.nextToken());
                this.assertToken(TokenType.SEMI_COLON_TKN, "Semi-colons are required at the end of break statements");
                breakStatement.endToken = this.nextToken(); // Consume semi-colon;
                block.hasJump = true;
                let enclosedFuncBreak = blockScope.getEnclosingScope(ScopeType.FUNC_SCOPE);
                let enclosedLoopBreak = blockScope.getEnclosingScope(ScopeType.LOOP_SCOPE);
                if (!enclosedLoopBreak) {
                    this.err.atWholeNode_PANIC("Break statement must be used within a loop", breakStatement);
                }
                if (enclosedFuncBreak && enclosedFuncBreak.scopeID > enclosedLoopBreak.scopeID) {
                    this.err.atWholeNode_PANIC("Can't break out of a function using a break statement", breakStatement);
                }
                return breakStatement;
            case TokenType.CONTINUE_TKN:
                let continueStatement = new AST.ASTContinue(this.nextToken());
                this.assertToken(
                    TokenType.SEMI_COLON_TKN,
                    "Semi-colons are required at the end of continue statements"
                );
                continueStatement.endToken = this.nextToken(); // Consume semi-colon;
                block.hasJump = true;
                let enclosedFuncCont = blockScope.getEnclosingScope(ScopeType.FUNC_SCOPE);
                let enclosedLoopCont = blockScope.getEnclosingScope(ScopeType.LOOP_SCOPE);
                if (!enclosedLoopCont) {
                    this.err.atWholeNode_PANIC("Continue statement must be used within a loop", continueStatement);
                }
                if (enclosedFuncCont && enclosedFuncCont.scopeID > enclosedLoopCont.scopeID) {
                    this.err.atWholeNode_PANIC(
                        "Can't continue out of a function using a continue statement",
                        continueStatement
                    );
                }
                return continueStatement;
            case TokenType.RETURN_TKN:
                let returnStatement = new AST.ASTReturn(this.nextToken());
                if (!this.checkToken(TokenType.SEMI_COLON_TKN)) {
                    returnStatement.returnValue = this.parseExpression(blockScope);
                }
                this.assertToken(TokenType.SEMI_COLON_TKN, "Semi-colons are required at the end of return statements");
                returnStatement.endToken = this.nextToken(); // Consume semi-colon;
                block.hasJump = true;
                block.hasReturn = true;
                return returnStatement;
            case TokenType.IF_TKN:
                let ifStmt = this.parseIf(blockScope);
                block.hasJump = ifStmt.hasJump;
                block.hasReturn = ifStmt.hasReturn;
                return ifStmt;
            case TokenType.WHILE_TKN:
                return this.parseWhile(blockScope);
            case TokenType.FOR_TKN:
                return this.parseFor(blockScope);
            default:
                let refName = this.parseExpression(blockScope);
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
                    return this.parseDeclarationFromName(refName, blockScope);
                }
        }
    }
    private parseWith(scope: Scope): AST.ASTExpression {
        this.nextToken(); //Consume with token
        this.assertIdentifier("a module name", "With keyword must be followed by a name reference to a module");
        let withModuleReference = this.parseExpression(scope);
        this.assertToken(TokenType.SEMI_COLON_TKN, "Semi-colons are required at the end of with statements");
        this.nextToken(); //Consume semi-colon
        return withModuleReference;
    }
    private parseIf(scope: Scope): AST.ASTIf {
        this.assertToken(TokenType.IF_TKN, "If statement must start with if keyword");
        let ifStatement = new AST.ASTIf(this.nextToken());
        let ifConseqScope = new Scope(scope);
        ifConseqScope.scopeType = ScopeType.BRANCH_SCOPE;
        ifStatement.condition = this.parseExpression(ifConseqScope);

        if (this.checkToken(TokenType.RIGHT_PARENS_TKN)) {
            this.err.atToken_PANIC("Unbalanced parenthesis. Expected {", this.peekToken());
        }
        ifStatement.consequence = this.parseBlock(ifConseqScope);
        ifStatement.endToken = ifStatement.consequence.endToken;
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
                let alternateScope = new Scope(scope);
                alternateScope.scopeType = ScopeType.BRANCH_SCOPE;
                ifStatement.alternative = this.parseBlock(alternateScope);
            }
            ifStatement.hasJump = ifStatement.consequence.hasJump && ifStatement.alternative.hasJump;
            ifStatement.hasReturn = ifStatement.consequence.hasReturn && ifStatement.alternative.hasReturn;
            ifStatement.endToken = ifStatement.alternative.endToken;
        }
        return ifStatement;
    }
    private parseWhile(scope: Scope): AST.ASTWhile {
        this.assertToken(TokenType.WHILE_TKN, "While loop must start with while keyword");
        let whileStatement = new AST.ASTWhile(this.nextToken());
        let whileScope = new Scope(scope);
        whileScope.scopeType = ScopeType.LOOP_SCOPE;
        whileStatement.condition = this.parseExpression(whileScope);
        whileStatement.block = this.parseBlock(whileScope);
        return whileStatement;
    }
    private parseFor(scope: Scope): AST.ASTFor {
        let forScope = new Scope(scope);
        forScope.scopeType = ScopeType.LOOP_SCOPE;
        this.assertToken(TokenType.FOR_TKN, "For loop must start with for keyword");
        let forStatement = new AST.ASTFor(this.nextToken());

        if (this.checkToken(TokenType.LEFT_PARENS_TKN)) {
            this.err.atToken_PANIC(
                "For loops don't allow encapsulating parenthesis in this language",
                this.peekToken()
            );
        }

        let indexDec = new AST.ASTDeclaration(this.peekToken());
        forStatement.indexParamDec = indexDec;
        indexDec.inScope = forScope;
        indexDec.lvalue = new AST.ASTName(this.peekToken());
        if (!this.checkToken(TokenType.IDENTIFIER_TKN)) {
            this.err.atToken_PANIC(
                "Invalid for loop. Expected a single variable name but got an expression instead",
                this.peekToken()
            );
        }
        indexDec.lvalue.refName = this.nextToken().stringValue;
        forScope.table.set(indexDec.lvalue.refName, indexDec);

        this.nextToken(); //Consume in

        let isLowerBoundExclusive = false;
        if (this.checkToken(TokenType.LEFT_PARENS_TKN)) {
            isLowerBoundExclusive = true;
        } else if (!this.checkToken(TokenType.LEFT_SQUARE_TKN)) {
            this.err.atToken_PANIC("Expected either a [ or a (", this.peekToken());
        }
        this.nextToken(); //consume ) or ]

        forStatement.lowerBound = this.parseExpression(forScope);
        if (isLowerBoundExclusive) {
            let expression = new AST.ASTBinaryOperator(forStatement.lowerBound.locToken);
            expression.lvalue = forStatement.lowerBound;
            expression.operation = new Token(
                "+",
                forStatement.lowerBound.locToken.line,
                forStatement.lowerBound.locToken.c,
                TokenType.OP_ADD_TKN
            );
            expression.rvalue = new AST.ASTLiteral(forStatement.lowerBound.locToken);
            expression.rvalue.value = Token.fromType(TokenType.NUMERIC_LITERAL_TKN, expression.rvalue.locToken);
            expression.rvalue.value.stringValue = "1";
            expression.rvalue.value.value = 1;
            forStatement.lowerBound = expression;
            expression.endToken = expression.rvalue.locToken;
        }

        if (this.checkToken(TokenType.RIGHT_SQUARE_TKN) || this.checkToken(TokenType.RIGHT_PARENS_TKN)) {
            this.err.atToken_PANIC("For loop range requires two expressions separated by a comma", this.peekToken());
        }
        this.assertToken(TokenType.COMMA_TKN, "Invalid declaration of a for loop range");
        this.nextToken();

        forStatement.upperBound = this.parseExpression(forScope);

        if (this.checkToken(TokenType.RIGHT_PARENS_TKN)) {
            let expression = new AST.ASTBinaryOperator(forStatement.upperBound.locToken);
            expression.lvalue = forStatement.upperBound;
            expression.operation = new Token(
                "-",
                forStatement.upperBound.locToken.line,
                forStatement.upperBound.locToken.c,
                TokenType.OP_SUBTR_TKN
            );
            expression.rvalue = new AST.ASTLiteral(forStatement.upperBound.locToken);
            expression.rvalue.value = Token.fromType(TokenType.NUMERIC_LITERAL_TKN, expression.rvalue.locToken);
            expression.rvalue.value.stringValue = "1";
            expression.rvalue.value.value = 1;
            forStatement.upperBound = expression;
            expression.endToken = expression.rvalue.locToken;
        } else if (!this.checkToken(TokenType.RIGHT_SQUARE_TKN)) {
            this.err.atToken_PANIC("Expected either a ] or a )", this.peekToken());
        }
        this.nextToken(); //consume ) or ]

        forStatement.block = this.parseBlock(forScope);
        return forStatement;
    }
    private parseDeclaration(scope: Scope): AST.ASTDeclaration {
        return this.parseDeclarationFromName(this.parseExpression(scope), scope);
    }
    private parseDeclarationFromName(refName: AST.ASTExpression, scope: Scope): AST.ASTDeclaration {
        let decl = new AST.ASTDeclaration(refName.locToken);
        decl.inScope = scope;
        decl.lvalue = refName;
        if (this.checkToken(TokenType.COLON_TKN)) {
            this.nextToken(); //Consume :
            decl.type = this.parseExpression(scope);
        }

        if (
            this.checkToken(TokenType.MUTABLE_ASSIGNMENT_TKN) ||
            this.checkToken(TokenType.IMMUTABLE_ASSIGNMENT_TKN) ||
            this.checkToken(TokenType.CONST_ASSIGNMENT_TKN)
        ) {
            decl.accessAssignment = this.nextToken();
            if (this.checkToken(TokenType.TYPE_TKN)) {
                decl.rvalue = this.parseTypeDefinition(scope);
                (<AST.ASTTypeDefinition>decl.rvalue).typeDecRef = decl;
                if (this.checkToken(TokenType.SEMI_COLON_TKN)) {
                    this.err
                        .atNode("Error in type definition", refName)
                        .atToken_PANIC(
                            "Semi-colons are not accepted directly after the end bracket of a type definition",
                            this.peekToken()
                        );
                }
            } else {
                decl.rvalue = this.parseExpression(scope);
                if (decl.rvalue.nodeName === AST.NodeType.MODULE) {
                    if (this.checkToken(TokenType.SEMI_COLON_TKN)) {
                        this.err
                            .atNode("Error in module", refName)
                            .atToken_PANIC(
                                "Semi-colons are not accepted directly after the end bracket of a module declaration",
                                this.peekToken()
                            );
                    }
                    (<AST.ASTModule>decl.rvalue).moduleDecRef = decl;
                } else if (decl.rvalue.nodeName === AST.NodeType.FUNCTION) {
                    if (this.checkToken(TokenType.SEMI_COLON_TKN)) {
                        this.err
                            .atNode("Error in function declaration", refName)
                            .atToken_PANIC(
                                "Semi-colons are not accepted directly after the end bracket of a function declaration",
                                this.peekToken()
                            );
                    }
                } else {
                    if (this.checkToken(TokenType.END_TKN)) {
                        this.err
                            .atWholeNode("Error in expression", decl.rvalue)
                            .atAfterLastToken_PANIC(
                                "Unterminated code at end of file. Expected an operator, operand, or terminating token like semi-colon",
                                this.peekToken()
                            );
                    } else if (this.checkToken(TokenType.RIGHT_PARENS_TKN)) {
                        this.err.atToken_PANIC("Unbalanced parenthesis. Expected a semi-colon", this.peekToken());
                    } else if (!this.checkToken(TokenType.SEMI_COLON_TKN)) {
                        this.err
                            .atNode("Error in declaration ", refName)
                            .atAfterLastToken_PANIC(
                                "Expected a semi-colon at the end of the declaration but got " +
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
            if (!scope.table.has((<AST.ASTName>refName).refName)) {
                let isFunc = decl.rvalue && decl.rvalue.nodeName === AST.NodeType.FUNCTION;
                if (isFunc && decl.accessAssignment.type === TokenType.MUTABLE_ASSIGNMENT_TKN) {
                    this.err
                        .atNode("Function can't be assigned using mutable assignment", decl)
                        .addNote_PANIC("Functions can only be assigned to a variable using = or =>");
                }
                scope.table.set((<AST.ASTName>refName).refName, decl);
                return decl;
            }
        }
        decl.isAssignment = true;
        return decl;
    }
    private parseModule(scope: Scope): AST.ASTModule {
        let moduleScope = new Scope(scope);
        moduleScope.scopeType = ScopeType.MODULE_SCOPE;
        this.assertToken(TokenType.MODULE_TKN, "Module must start with module keyword");
        let module = new AST.ASTModule(this.nextToken());
        module.scope = moduleScope;
        module.declarations = [];
        this.assertToken(TokenType.LEFT_CURLY_TKN);
        this.nextToken(); //Consume left curly
        while (!this.checkToken(TokenType.RIGHT_CURLY_TKN) && !this.checkToken(TokenType.END_TKN)) {
            module.declarations.push(this.parseDeclaration(moduleScope));
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
    private parseTypeDefinition(scope: Scope): AST.ASTTypeDefinition {
        let typeDefScope = new Scope(scope);
        typeDefScope.scopeType = ScopeType.TYPE_SCOPE;
        this.assertToken(TokenType.TYPE_TKN, "Type definition must start with type keyword");
        let typeDef = new AST.ASTTypeDefinition(this.nextToken());
        typeDef.scope = typeDefScope;
        typeDef.declarations = [];
        this.assertToken(TokenType.LEFT_CURLY_TKN, "Type definition must be enclosed in curly brackets");
        this.nextToken(); //Consume left curly
        while (!this.checkToken(TokenType.RIGHT_CURLY_TKN) && !this.checkToken(TokenType.END_TKN)) {
            typeDef.declarations.push(this.parseDeclaration(typeDefScope));
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
    private tryParseFunction(scope: Scope): AST.ASTFunction | undefined {
        let parensAndIdentifier =
            this.checkAheadToken(1, TokenType.LEFT_PARENS_TKN) && this.checkAheadToken(2, TokenType.IDENTIFIER_TKN);
        if (
            !(
                (parensAndIdentifier &&
                    (this.checkAheadToken(3, TokenType.COLON_TKN) ||
                        this.checkAheadToken(3, TokenType.MUTABLE_ASSIGNMENT_TKN))) ||
                (this.checkAheadToken(1, TokenType.LEFT_PARENS_TKN) &&
                    this.checkAheadToken(2, TokenType.RIGHT_PARENS_TKN))
            )
        ) {
            if (
                parensAndIdentifier &&
                (this.checkAheadToken(3, TokenType.CONST_ASSIGNMENT_TKN) ||
                    this.checkAheadToken(3, TokenType.IMMUTABLE_ASSIGNMENT_TKN))
            ) {
                this.err.atToken_PANIC(
                    "Default function parameters can only be mutable assigned with ~=",
                    this.lookAheadToken(3)
                );
            }
            return undefined;
        }

        let functionScope = new Scope(scope);
        functionScope.scopeType = ScopeType.FUNC_SCOPE;
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
            let otherDec = functionScope.table.get((<AST.ASTName>paramDec.lvalue).refName);
            if (otherDec) {
                this.err
                    .atNode("Found duplicate name for parameter declaration", paramDec)
                    .atNode_PANIC("Other duplicate parameter here", otherDec);
            } else {
                functionScope.table.set(paramDec.lvalue.refName, paramDec);
            }
            if (this.checkToken(TokenType.COLON_TKN)) {
                this.nextToken(); //Consume colon
                paramDec.type = this.parseExpression(functionScope);
            }
            if (this.checkToken(TokenType.MUTABLE_ASSIGNMENT_TKN)) {
                paramDec.accessAssignment = this.nextToken(); //Consume =
                paramDec.rvalue = this.parseExpression(functionScope);
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
                if (this.checkAheadToken(2, TokenType.RIGHT_PARENS_TKN)) {
                    // a = (b: num,  )
                    //             ^
                    this.err.atAfterLastToken_PANIC(
                        "Expected another parameter definition after the comma but got nothing",
                        this.lookAheadToken(2)
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
            func.returnType = this.parseExpression(functionScope);
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
    private parseExpression(scope: Scope): AST.ASTExpression {
        return this.recurseExpression(this.LOWEST_PRECEDENCE, 0, { token: this.peekToken(), scope: scope });
    }
    private recurseExpression(
        precedence: number,
        depthCount: number,
        src: { token: Token; scope: Scope }
    ): AST.ASTExpression {
        let expression!: AST.ASTExpression;
        while (!this.checkToken(TokenType.END_TKN)) {
            let tkn = this.peekToken();
            if (expression) {
                switch (tkn.type) {
                    case TokenType.MODULE_TKN:
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
                    default:
                        let curPrecedence = this.precedence.get(tkn.type);
                        if (curPrecedence !== undefined) {
                            if (curPrecedence > precedence) {
                                if (this.checkToken(TokenType.DOT_TKN)) {
                                    if (this.checkAheadToken(2, TokenType.LEFT_CURLY_TKN)) {
                                        expression = this.parseTypeConstruction(expression, src.scope);
                                        break;
                                    }
                                    let dotOp = new AST.ASTDotOperator(expression.locToken);
                                    dotOp.rootValue = expression;
                                    this.nextToken(); //Consume the dot token
                                    let memberVal = this.recurseExpression(curPrecedence, depthCount, src);
                                    if (memberVal.nodeName !== AST.NodeType.NAME) {
                                        this.err.atWholeNode_PANIC(
                                            "Expected a variable name reference to a property in " +
                                                AST.exprToStr(dotOp.rootValue),
                                            memberVal
                                        );
                                    }
                                    dotOp.memberValue = <AST.ASTName>memberVal;
                                    expression = dotOp;
                                    dotOp.endToken = dotOp.memberValue.endToken;
                                } else if (this.checkToken(TokenType.LEFT_PARENS_TKN)) {
                                    expression = this.parseCall(expression, src);
                                } else {
                                    if (this.checkToken(TokenType.BIN_SHIFT_LEFT_TKN)) {
                                        if (this.checkAheadToken(2, TokenType.COND_LESS_THAN_TKN)) {
                                            this.err.atToken_PANIC(
                                                "Arithmetic left shift is equivalent to logical left shift. Use << instead of <<<",
                                                this.peekToken()
                                            );
                                        }
                                    }
                                    let binOp = new AST.ASTBinaryOperator(expression.locToken);
                                    binOp.lvalue = expression;
                                    binOp.operation = this.nextToken();
                                    binOp.rvalue = this.recurseExpression(curPrecedence, depthCount, src);
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
                                    .atToken("Error in expression starting with", src.token)
                                    .atToken_PANIC("Unbalanced parenthesis. Expected " + depthCount + " more )", tkn); //b = (((((0 + 5)
                            } else {
                                return expression;
                            }
                        }
                }
            } else {
                switch (tkn.type) {
                    case TokenType.MODULE_TKN:
                        expression = this.parseModule(src.scope);
                        break;
                    case TokenType.MUT_CAST_TKN:
                    case TokenType.CONST_CAST_TKN:
                        expression = new AST.ASTAccessCast(tkn);
                        expression.accessType = <AST.AccessCastToken>this.nextToken().type;
                        expression.castedType = this.recurseExpression(this.HIGHEST_PRECEDENCE, depthCount, src);
                        expression.endToken = expression.castedType.endToken;
                        break;
                    case TokenType.NUM_TYPE_TKN:
                    case TokenType.STRING_TYPE_TKN:
                    case TokenType.BOOL_TYPE_TKN:
                    case TokenType.VOID_TYPE_TKN:
                    case TokenType.TYPE_TKN:
                        expression = new AST.ASTTypeLiteral(tkn);
                        expression.type = <AST.TypeLiteralToken>this.nextToken().type;
                        expression.endToken = tkn;
                        break;
                    case TokenType.NUMERIC_LITERAL_TKN:
                    case TokenType.STRING_LITERAL_TKN:
                    case TokenType.COND_TRUE_TKN:
                    case TokenType.COND_FALSE_TKN:
                        expression = new AST.ASTLiteral(tkn);
                        expression.value = this.nextToken();
                        expression.endToken = tkn;
                        break;
                    case TokenType.COND_NOT_TKN:
                    case TokenType.BIN_NOT_TKN:
                        expression = new AST.ASTUnaryOperator(tkn);
                        expression.operation = this.nextToken();
                        expression.value = this.recurseExpression(precedence, depthCount, src);
                        expression.endToken = expression.value.endToken;
                        break;
                    case TokenType.BACKSLASH_TKN:
                        expression = this.parseFunctionType(src.scope);
                        break;
                    case TokenType.IDENTIFIER_TKN:
                        expression = new AST.ASTName(this.nextToken());
                        expression.refName = expression.locToken.stringValue;
                        expression.endToken = expression.locToken;
                        break;
                    case TokenType.LEFT_PARENS_TKN:
                        let func = this.tryParseFunction(src.scope);
                        if (func) {
                            expression = func;
                        } else {
                            //This is an open parenthesis
                            this.nextToken(); //Consume left parenthesis
                            expression = this.recurseExpression(this.LOWEST_PRECEDENCE, depthCount + 1, src);
                            this.nextToken(); //Consume right parenthesis
                        }
                        break;
                    case TokenType.DOT_TKN:
                        this.err
                            .atToken("Error in reference starting with", src.token)
                            .atToken_PANIC(
                                "Can't start an expression with a dot or have two dots in a row",
                                this.peekToken()
                            );
                    case TokenType.OP_SUBTR_TKN:
                        // This is negation
                        expression = new AST.ASTBinaryOperator(tkn);
                        expression.lvalue = new AST.ASTLiteral(tkn);
                        expression.lvalue.value = Token.fromType(TokenType.NUMERIC_LITERAL_TKN, tkn);
                        expression.lvalue.value.stringValue = "0";
                        expression.lvalue.value.value = 0;
                        expression.operation = this.nextToken();
                        expression.rvalue = this.recurseExpression(this.HIGHEST_PRECEDENCE, depthCount, src);
                        expression.endToken = expression.rvalue.endToken;
                        break;
                    default:
                        this.err.atAfterLastToken_PANIC("Expected an expression but got nothing", tkn); //a = 3 + ;
                }
            }
        }
        return expression;
    }
    private parseCall(prevExpression: AST.ASTExpression, src: { token: Token; scope: Scope }): AST.ASTCall {
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
                call.givenParams.push(this.recurseExpression(this.LOWEST_PRECEDENCE, 0, src));
            }
        }
        this.nextToken(); // Consume right parenthesis
        return call;
    }

    private parseTypeConstruction(prevExpression: AST.ASTExpression, scope: Scope): AST.ASTTypeConstruction {
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
                rvalue: this.parseExpression(scope),
            };
            typeConstruct.assignments.push(assignment);
            if (this.checkToken(TokenType.COMMA_TKN)) {
                if (this.checkAheadToken(2, TokenType.RIGHT_CURLY_TKN)) {
                    // particle = Particle { a = 3,   };
                    //                            ^
                    this.err.atToken_PANIC(
                        "Last assignment in a type construction can't end in a comma",
                        this.peekToken()
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
        typeConstruct.endToken = this.nextToken(); //Consume right curly token
        return typeConstruct;
    }
    private parseFunctionType(scope: Scope): AST.ASTFunctionType {
        this.assertToken(TokenType.BACKSLASH_TKN, "Function type must start with a backslash");
        this.nextToken();

        let type = new AST.ASTFunctionType(this.peekToken());
        this.nextToken();
        type.inputType = [];
        while (!this.checkToken(TokenType.RIGHT_PARENS_TKN) && !this.checkToken(TokenType.END_TKN)) {
            type.inputType.push(this.parseExpression(scope));
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
        type.outType = this.parseExpression(scope);
        return type;
    }
}
