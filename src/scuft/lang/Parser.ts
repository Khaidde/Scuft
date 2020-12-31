import { ErrorHandler } from "./ErrorHandler";
import { Token, TokenType } from "./Token";
import * as AST from "./Ast";

import Lexer from "./Lexer";
import { Scope } from "./Scope";

export default class Parser {
    private readonly lexer: Lexer;
    globalScope!: Scope;
    readonly err: ErrorHandler;
    constructor(lexer: Lexer, errHandler: ErrorHandler) {
        this.lexer = lexer;
        this.err = ErrorHandler.fromHandler("----Parser----\n", errHandler);

        this.initPrecedenceMap();
    }
    private assertToken(type: TokenType, expectedTkn: string, msg?: string) {
        let tkn = this.peekToken();
        if (tkn.type !== type) {
            let res = "Expected ";
            if (expectedTkn) {
                res += expectedTkn;
            } else {
                res += TokenType[type];
            }
            if (tkn.type === TokenType.IDENTIFIER_TKN) {
                res += " but got '" + tkn.stringValue + "' instead";
            } else {
                res += " but got " + tkn.stringValue + " instead";
            }
            if (msg) {
                res += ": " + msg;
            }
            this.err.atAfterLastToken_PANIC(res, tkn);
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
        program.statements = [];
        while (!this.checkToken(TokenType.END_TKN)) {
            switch (this.peekToken().type) {
                case TokenType.MODULE_TKN:
                    this.err.atToken_PANIC(
                        "Must declare a module in the format [moduleName] = module {...}",
                        this.peekToken()
                    );
                case TokenType.TYPE_TKN:
                    this.err.atToken_PANIC(
                        "Must declare a type definition in the format [typeName] = type {...}",
                        this.peekToken()
                    );
                case TokenType.WITH_TKN:
                    this.err.atToken_PANIC("Can't use 'with' statement in global scope", this.peekToken());
                default:
                    program.statements.push(this.parseStatement(this.globalScope));
            }
        }
        return program;
    }
    private parseBlock(blockScope: Scope): AST.ASTBlock {
        this.assertToken(TokenType.LEFT_CURLY_TKN, "{", "Block must start wth {");
        let block = new AST.ASTBlock(this.nextToken());
        block.withModules = [];
        block.statements = [];
        while (!this.checkToken(TokenType.RIGHT_CURLY_TKN) && !this.checkToken(TokenType.END_TKN)) {
            switch (this.peekToken().type) {
                case TokenType.MODULE_TKN:
                    this.err.atToken_PANIC(
                        "Must declare a module in the format [moduleName] = module {...}",
                        this.peekToken()
                    );
                case TokenType.TYPE_TKN:
                    this.err.atToken_PANIC(
                        "Must declare a type definition in the format [typeName] = type {...}",
                        this.peekToken()
                    );
                case TokenType.WITH_TKN:
                    block.withModules.push(this.parseWith(blockScope));
                    break;
                default:
                    block.statements.push(this.parseStatement(blockScope));
                    break;
            }
        }
        if (this.checkToken(TokenType.END_TKN)) {
            this.err
                .atToken("Error in block", block.locToken)
                .atAfterLastToken_PANIC("Unterminated code at end of file. Expected closing }", this.peekToken());
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
                this.assertToken(
                    TokenType.SEMI_COLON_TKN,
                    ";",
                    "Semi-colons are required at the end of break statements"
                );
                this.nextToken();
                return breakStatement;
            case TokenType.CONTINUE_TKN:
                let continueStatement = new AST.ASTContinue(this.nextToken());
                this.assertToken(
                    TokenType.SEMI_COLON_TKN,
                    ";",
                    "Semi-colons are required at the end of continue statements"
                );
                this.nextToken();
                return continueStatement;
            case TokenType.RETURN_TKN:
                let returnStatement = new AST.ASTReturn(this.nextToken());
                if (!this.checkToken(TokenType.SEMI_COLON_TKN)) {
                    returnStatement.returnValue = this.parseExpression();
                }
                this.assertToken(
                    TokenType.SEMI_COLON_TKN,
                    ";",
                    "Semi-colons are required at the end of return statements"
                );
                this.nextToken();
                return returnStatement;
            case TokenType.LEFT_PARENS_TKN:
            case TokenType.IDENTIFIER_TKN:
                let refName = this.parseReference();
                if (this.checkToken(TokenType.SEMI_COLON_TKN)) {
                    this.assertToken(
                        TokenType.SEMI_COLON_TKN,
                        ";",
                        "Semi-colons are required at the end of statements"
                    );
                    this.nextToken(); // Consume semi-colon
                    return refName;
                } else {
                    let dec = this.parseDeclarationFromName(scope, refName);
                    this.assertSemiColonAfterDeclaration(dec);
                    return dec;
                }
            default:
                this.err.atToken_PANIC(
                    "Declaration or expression must start with a reference to a name",
                    this.peekToken()
                );
        }
    }
    private parseDeclaration(scope: Scope): AST.ASTDeclaration {
        if (!this.checkToken(TokenType.IDENTIFIER_TKN)) {
            this.err.atToken_PANIC("The expected declaration must start with a variable name", this.peekToken());
        }
        return this.parseDeclarationFromName(scope, this.parseReference());
    }
    private parseDeclarationFromName(scope: Scope, identifier: AST.ASTReference): AST.ASTDeclaration {
        let declaration = new AST.ASTDeclaration(identifier.locToken);
        declaration.lvalue = identifier;
        if (declaration.lvalue.nodeName == AST.NodeType.NAME) {
            let name = <AST.ASTName>declaration.lvalue;
            let attemptAdded = scope.attemptAdd(name.refName, declaration);
            if (!attemptAdded.locToken.equals(declaration.locToken)) {
                this.err
                    .atToken("Found duplicate name declaration here", attemptAdded.locToken)
                    .atToken_PANIC("Other duplicate name declaration here ", declaration.locToken);
            }
        } else {
            this.err.atToken("Handle complex declaration names", declaration.locToken).warn();
        }

        if (this.checkToken(TokenType.COLON_TKN)) {
            this.nextToken();
            declaration.type = this.parseType();
        }
        if (this.checkToken(TokenType.ASSIGNMENT_TKN)) {
            this.nextToken();

            switch (this.peekToken().type) {
                case TokenType.TYPE_TKN:
                    declaration.rvalue = this.parseTypeDefinition(scope);
                    break;
                case TokenType.MODULE_TKN:
                    declaration.rvalue = this.parseModule(scope);
                    break;
                default:
                    //Distinguish function declaration from variable declaration
                    if (
                        (this.lookAheadToken(1).type === TokenType.LEFT_PARENS_TKN &&
                            this.lookAheadToken(2).type === TokenType.IDENTIFIER_TKN &&
                            this.lookAheadToken(3).type === TokenType.COLON_TKN) ||
                        (this.lookAheadToken(1).type === TokenType.LEFT_PARENS_TKN &&
                            this.lookAheadToken(2).type === TokenType.RIGHT_PARENS_TKN &&
                            (this.lookAheadToken(3).type === TokenType.ARROW_TKN ||
                                this.lookAheadToken(3).type === TokenType.LEFT_CURLY_TKN))
                    ) {
                        declaration.rvalue = this.parseFunction(scope);
                    } else {
                        declaration.rvalue = this.parseExpression();
                    }
            }
        }
        if (!declaration.type && !declaration.rvalue) {
            // a for
            //   ^
            this.err
                .atToken("Error in declaration", declaration.locToken)
                .atToken_PANIC(
                    this.peekToken().stringValue +
                        " is not allowed here. Declaration must either assign a value using = or specify a type using :",
                    this.peekToken()
                );
        }
        return declaration;
    }
    private assertSemiColonAfterDeclaration(declaration: AST.ASTDeclaration) {
        if (
            declaration.rvalue &&
            (declaration.rvalue.nodeName === AST.NodeType.FUNCTION ||
                declaration.rvalue.nodeName === AST.NodeType.MODULE ||
                declaration.rvalue.nodeName === AST.NodeType.TYPE_DEF)
        ) {
            if (this.checkToken(TokenType.SEMI_COLON_TKN)) {
                switch (declaration.rvalue.nodeName) {
                    case AST.NodeType.MODULE:
                        this.err
                            .atToken("Error in module declaration", declaration.locToken)
                            .atToken_PANIC("Semi-colons are not accepted after a module declaration", this.peekToken());
                    case AST.NodeType.TYPE_DEF:
                        this.err
                            .atToken("Error in type definition", declaration.locToken)
                            .atToken_PANIC("Semi-colons are not accepted after a type definition", this.peekToken());
                    case AST.NodeType.FUNCTION:
                        this.err
                            .atToken("Error in function declaration", declaration.locToken)
                            .atToken_PANIC(
                                "Semi-colons are not accepted after a function declaration",
                                this.peekToken()
                            );
                }
            }
        } else {
            if (this.checkToken(TokenType.RIGHT_PARENS_TKN)) {
                this.err.atToken_PANIC("Unbalanced parenthesis. Expected a semi-colon", this.peekToken());
            }
            if (!this.checkToken(TokenType.SEMI_COLON_TKN)) {
                this.err
                    .atToken("Error in declaration", declaration.lvalue.locToken)
                    .atAfterLastToken_PANIC(
                        "Expected a semi-colon at the end of the variable declaration but got " +
                            this.peekToken().stringValue +
                            " instead",
                        this.peekToken()
                    );
            }
            this.nextToken();
        }
    }
    private parseWith(scope: Scope): AST.ASTReference {
        //TODO do stuff to find the with module and add it to the current scope

        this.nextToken(); //Consume with token
        this.assertToken(
            TokenType.IDENTIFIER_TKN,
            "a module name",
            "With keyword must be followed by a name reference to a module"
        );
        let withModuleReference = this.parseReference();
        this.assertToken(TokenType.SEMI_COLON_TKN, ";", "Semi-colons are required at the end of with statements");
        this.nextToken(); //Consume semi-colon
        return withModuleReference;
    }
    private parseReference(): AST.ASTReference {
        return this.recurseReference(this.peekToken());
    }
    private recurseReference(sourceToken: Token): AST.ASTReference {
        let reference!: AST.ASTReference;
        while (!this.checkToken(TokenType.END_TKN)) {
            let tkn = this.peekToken();
            if (reference) {
                switch (tkn.type) {
                    case TokenType.IDENTIFIER_TKN:
                        this.err
                            .atToken("Error in reference starting with", sourceToken)
                            .atToken_PANIC(
                                "A dot must be used to separate a name from the reference to the left of it",
                                tkn
                            );
                    case TokenType.LEFT_PARENS_TKN:
                        reference = this.parseCall(reference, sourceToken);
                        continue;
                    case TokenType.DOT_TKN:
                        let dotOp = new AST.ASTDotOperator(reference.locToken);
                        dotOp.rootValue = reference;
                        this.nextToken(); //Consume the dot token
                        dotOp.memberValue = this.recurseReference(sourceToken);
                        reference = dotOp;
                        continue;
                    default:
                        return reference;
                }
            } else {
                switch (tkn.type) {
                    case TokenType.IDENTIFIER_TKN:
                        reference = new AST.ASTName(this.nextToken());
                        reference.refName = reference.locToken.stringValue;
                        continue;
                    case TokenType.DOT_TKN:
                        this.err
                            .atToken("Error in reference starting with", sourceToken)
                            .atToken_PANIC(
                                "Can't start a reference with a dot or have two dots in a row",
                                this.peekToken()
                            );
                    default:
                        this.err
                            .atToken("Error in reference starting with", sourceToken)
                            .atToken_PANIC(
                                "Reference access must only contain names, dots and calls",
                                this.peekToken()
                            );
                }
            }
        }
        this.err
            .atToken("Error in reference starting with", sourceToken)
            .atAfterLastToken_PANIC(
                "Unterminated code at end of file. Expected another name, dot or terminating token like semi-colon",
                this.peekToken()
            );
        return reference;
    }
    private parseIf(scope: Scope): AST.ASTIf {
        this.assertToken(TokenType.IF_TKN, "if", "If statement must start with if keyword");
        let ifStatement = new AST.ASTIf(this.nextToken());
        ifStatement.condition = this.parseExpression();

        if (this.checkToken(TokenType.RIGHT_PARENS_TKN))
            this.err.atToken_PANIC("Unbalanced parenthesis. Expected {", this.peekToken());
        ifStatement.consequence = this.parseBlock(Scope.newScopeFrom(scope));
        if (this.checkToken(TokenType.ELSE_TKN)) {
            this.nextToken(); //Consume else token
            if (this.checkToken(TokenType.IF_TKN)) {
                ifStatement.alternative = this.parseIf(scope);
            } else if (this.checkToken(TokenType.LEFT_CURLY_TKN)) {
                ifStatement.alternative = this.parseBlock(Scope.newScopeFrom(scope));
            } else if (this.checkToken(TokenType.END_TKN)) {
                this.err
                    .atToken("Error in if statement", ifStatement.locToken)
                    .atAfterLastToken_PANIC(
                        "Unterminated code at end of file. Expected if keyword or {",
                        this.peekToken()
                    );
            } else {
                this.err.atToken_PANIC(
                    this.peekToken().stringValue +
                        " is not allowed here. Expected either an if keyword to create an else if or a { to declare a block",
                    this.peekToken()
                );
            }
        }
        return ifStatement;
    }
    private parseWhile(scope: Scope): AST.ASTWhile {
        this.assertToken(TokenType.WHILE_TKN, "while", "While loop must start with while keyword");
        let whileStatement = new AST.ASTWhile(this.nextToken());
        whileStatement.condition = this.parseExpression();

        whileStatement.block = this.parseBlock(Scope.newScopeFrom(scope));
        return whileStatement;
    }
    private parseFor(scope: Scope): AST.ASTFor {
        this.assertToken(TokenType.FOR_TKN, "for", "For loop must start with for keyword");
        let forStatement = new AST.ASTFor(this.nextToken());

        if (this.checkToken(TokenType.LEFT_PARENS_TKN)) {
            this.err.atToken_PANIC(
                "For loops don't require encapsulating parenthesis in this language",
                this.peekToken()
            );
        }
        switch (this.lookAheadToken(2).type) {
            case TokenType.LEFT_CURLY_TKN:
                this.assertToken(TokenType.IDENTIFIER_TKN, "a variable reference to an iterable");
                forStatement.iterableName = this.parseExpression();
                break;
            case TokenType.COMMA_TKN:
                forStatement.itemParamDec = new AST.ASTName(this.peekToken());
                this.assertToken(TokenType.IDENTIFIER_TKN, "an 'item' variable declaration");
                forStatement.itemParamDec.refName = this.nextToken().stringValue;
                this.nextToken(); //Consume comma
                forStatement.indexParamDec = new AST.ASTName(this.peekToken());
                this.assertToken(TokenType.IDENTIFIER_TKN, "an 'index' variable declaration");
                forStatement.indexParamDec.refName = this.nextToken().stringValue;
                this.assertToken(TokenType.IN_TKN, "in");
                this.nextToken(); //Consume in token
                this.assertToken(TokenType.IDENTIFIER_TKN, "a variable reference to an iterable");
                forStatement.iterableName = this.parseExpression();
                break;
            case TokenType.IN_TKN:
                forStatement.itemParamDec = new AST.ASTName(this.peekToken());
                this.assertToken(TokenType.IDENTIFIER_TKN, "an 'item' variable declaration");
                forStatement.itemParamDec.refName = this.nextToken().stringValue;
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
                        expression.rvalue = this.makeNewLiteralNode(forStatement.lowerBound.locToken, 1);
                        forStatement.lowerBound = expression;
                    }

                    if (this.checkToken(TokenType.RIGHT_SQUARE_TKN) || this.checkToken(TokenType.RIGHT_PARENS_TKN)) {
                        this.err.atToken_PANIC(
                            "For loop range requires two expressions separated by a comma",
                            this.peekToken()
                        );
                    }
                    this.assertToken(TokenType.COMMA_TKN, ",", "Invalid declaration of a for loop range");
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
                        expression.rvalue = this.makeNewLiteralNode(forStatement.upperBound.locToken, 1);
                        forStatement.upperBound = expression;
                    } else if (!this.checkToken(TokenType.RIGHT_SQUARE_TKN)) {
                        this.err.atToken_PANIC("Expected either a ] or a )", this.peekToken());
                    }
                    this.nextToken(); //consume ) or ]
                    break;
                } else {
                    this.assertToken(
                        TokenType.IDENTIFIER_TKN,
                        "a variable reference to an iterable",
                        "A potential fix is to use #range after the in keyword to denote iteration over a range"
                    );
                    forStatement.iterableName = this.parseExpression();
                    break;
                }
            default:
                this.err.atToken_PANIC(
                    "Invalid for loop. Expected a variable name but got " + this.peekToken().stringValue + " instead",
                    this.peekToken()
                );
        }
        forStatement.block = this.parseBlock(Scope.newScopeFrom(scope));
        return forStatement;
    }
    private parseModule(scope: Scope): AST.ASTModule {
        let moduleScope = Scope.newScopeFrom(scope);
        this.assertToken(TokenType.MODULE_TKN, "module", "Module must start with module keyword");
        let module = new AST.ASTModule(this.nextToken());

        module.withModules = [];
        module.declarations = [];
        this.assertToken(TokenType.LEFT_CURLY_TKN, "{");
        this.nextToken(); //Consume left curly
        while (!this.checkToken(TokenType.RIGHT_CURLY_TKN) && !this.checkToken(TokenType.END_TKN)) {
            if (this.checkToken(TokenType.WITH_TKN)) {
                module.withModules.push(this.parseWith(scope));
            } else {
                if (!this.checkToken(TokenType.IDENTIFIER_TKN)) {
                    this.err
                        .atToken("Error in module", module.locToken)
                        .atToken_PANIC("Module must only contain variable declarations", this.peekToken());
                }
                let declaration = this.parseDeclaration(moduleScope);
                if (declaration.rvalue && declaration.nodeName === AST.NodeType.MODULE) {
                    //TODO this may or may not change
                    this.err.atToken_PANIC("Module declaration must be declared in global scope", this.peekToken());
                }
                module.declarations.push(declaration);
                this.assertSemiColonAfterDeclaration(declaration);
            }
        }
        if (this.checkToken(TokenType.END_TKN)) {
            this.err
                .atToken("Error in module", module.locToken)
                .atAfterLastToken_PANIC("Unterminated code at end of file. Expected closing {", this.peekToken());
        } else {
            this.nextToken();
        }
        return module;
    }
    private parseTypeDefinition(scope: Scope): AST.ASTTypeDefinition {
        let typeDefScope = Scope.newTypeDefScopeFrom(scope);
        this.assertToken(TokenType.TYPE_TKN, "type", "Type definition must start with type keyword");
        let typeDef = new AST.ASTTypeDefinition(this.nextToken());

        typeDef.withModules = [];
        typeDef.declarations = [];
        this.assertToken(TokenType.LEFT_CURLY_TKN, "{", "Type definition must be enclosed in curly brackets");
        this.nextToken(); //Consume left curly
        while (!this.checkToken(TokenType.RIGHT_CURLY_TKN) && !this.checkToken(TokenType.END_TKN)) {
            if (this.checkToken(TokenType.WITH_TKN)) {
                typeDef.withModules.push(this.parseWith(typeDefScope));
            } else {
                if (!this.checkToken(TokenType.IDENTIFIER_TKN)) {
                    this.err
                        .atToken("Error in type definition", typeDef.locToken)
                        .atToken_PANIC("Type definition must only contain variable declarations", this.peekToken());
                }
                let declaration = this.parseDeclaration(typeDefScope);
                if (declaration.lvalue.nodeName !== AST.NodeType.NAME) {
                    this.err.atToken_PANIC(
                        "The variable name declaration of a type definition can't have dots",
                        declaration.locToken
                    );
                }
                typeDef.declarations.push(<AST.ASTSingleVarDeclaration>declaration);
                if (declaration.rvalue) {
                    if (declaration.nodeName === AST.NodeType.MODULE) {
                        this.err
                            .atToken("Error in type definition", typeDef.locToken)
                            .atToken_PANIC("Modules are not allowed within type definitions", declaration.locToken);
                    }
                    if (declaration.nodeName === AST.NodeType.TYPE_DEF) {
                        this.err
                            .atToken("Error in type definition", typeDef.locToken)
                            .atToken_PANIC(
                                "Type definitions are not allowed within type definitions",
                                declaration.locToken
                            );
                    }
                }
                this.assertSemiColonAfterDeclaration(declaration);
            }
        }
        if (this.checkToken(TokenType.END_TKN)) {
            this.err
                .atToken("Error in type definition", typeDef.locToken)
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

        this.assertToken(TokenType.LEFT_PARENS_TKN, "(", "Function declaration must specify parameters or use ()");
        this.nextToken(); //Consume left parenthesis
        while (!this.checkToken(TokenType.RIGHT_PARENS_TKN) && !this.checkToken(TokenType.END_TKN)) {
            let declaration = this.parseDeclaration(functionScope);
            if (declaration.lvalue.nodeName !== AST.NodeType.NAME)
                this.err.atToken_PANIC(
                    "The variable name declaration of a function parameter can't have dots",
                    declaration.locToken
                );
            func.paramDeclaration.push(<AST.ASTSingleVarDeclaration>declaration);
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
        if (this.checkToken(TokenType.LEFT_CURLY_TKN)) {
            func.block = this.parseBlock(functionScope);
        } else {
            //a = (b: num) for { }
            //             ^
            this.err.atToken_PANIC(
                this.peekToken().stringValue +
                    " is not allowed here. Function must either declare a return type using -> or omit completely",
                this.peekToken()
            );
        }

        return func;
    }
    private parseType(): AST.ASTType {
        let expression = this.parseExpression();
        switch (expression.nodeName) {
            case AST.NodeType.NAME:
            case AST.NodeType.DOT_OP:
            case AST.NodeType.CALL:
            case AST.NodeType.TYPE_LITERAL:
            case AST.NodeType.FUNCTION_TYPE:
                return <AST.ASTType>expression;
        }
        this.err.atToken_PANIC("Invalid type expression", expression.locToken);
    }

    private precedence: Map<TokenType, number> = new Map();
    readonly LOWEST_PRECEDENCE: number = 0;
    readonly HIGHEST_PRECEDENCE: number = 10;
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

        this.setPrecedence(13, TokenType.LEFT_PARENS_TKN); // For lambda calls. Reference calls are handled in "parseReference"
    }
    private makeNewLiteralNode(locToken: Token, value: number | string | boolean): AST.ASTLiteral {
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
        }
        let literal = new AST.ASTLiteral(tkn);
        literal.value = tkn;
        return literal;
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
                            "Operations on a module declaration are illegal. Modules should strictly be assigned to a variable",
                            tkn
                        );
                case TokenType.TYPE_TKN:
                    this.err
                        .atToken("Error in expression starting with", sourceToken)
                        .atToken_PANIC(
                            "Operations on a type definition are illegal. Type definitions should strictly be assigned to a variable",
                            tkn
                        );
            }
            if (expression) {
                switch (tkn.type) {
                    case TokenType.NUM_TYPE_TKN:
                    case TokenType.STRING_TYPE_TKN:
                    case TokenType.BOOL_TYPE_TKN:
                    case TokenType.VOID_TYPE_TKN:
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
                        } else {
                            this.err
                                .atToken("Error in expression starting with", sourceToken)
                                .atToken_PANIC("Dot operator can only be used between two references", tkn);
                        }
                    default:
                        let curPrecedence = this.precedence.get(tkn.type);
                        if (curPrecedence !== undefined) {
                            if (curPrecedence > precedence) {
                                if (this.checkToken(TokenType.LEFT_PARENS_TKN)) {
                                    expression = this.parseCall(expression, sourceToken);
                                } else {
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
                            if (this.peekToken().type === TokenType.UNKNOWN_TKN) {
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
                    case TokenType.NUM_TYPE_TKN:
                    case TokenType.STRING_TYPE_TKN:
                    case TokenType.BOOL_TYPE_TKN:
                    case TokenType.VOID_TYPE_TKN:
                        expression = new AST.ASTTypeLiteral(tkn);
                        expression.type = this.nextToken();
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
                        expression = this.recurseReference(sourceToken);
                        break;
                    case TokenType.LEFT_PARENS_TKN:
                        //This is an open parenthesis
                        this.nextToken(); //Consume left parenthesis
                        expression = this.recurseExpression(this.LOWEST_PRECEDENCE, depthCount + 1, sourceToken);
                        this.nextToken(); //Consume right parenthesis
                        break;
                    case TokenType.OP_SUBTR_TKN:
                        // This is negation
                        expression = new AST.ASTBinaryOperator(tkn);
                        expression.lvalue = this.makeNewLiteralNode(tkn, 0);
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
        this.assertToken(TokenType.DOT_TKN, ".", "dot '.' is required after the type name in a type construction");
        this.nextToken(); //Consume dot
        this.assertToken(TokenType.LEFT_CURLY_TKN, "{", "Type construction must be enclosed in curly brackets");
        this.nextToken(); //Consume left curly
        typeConstruct.assignments = [];
        while (!this.checkToken(TokenType.RIGHT_CURLY_TKN)) {
            let declaration = new AST.ASTDeclaration(this.peekToken());
            declaration.lvalue = this.parseReference();
            if (declaration.lvalue.nodeName !== AST.NodeType.NAME) {
                this.err
                    .atToken("Error in type constructor", typeConstruct.locToken)
                    .atToken_PANIC(
                        "Type constructor assignments need to be single variable names",
                        declaration.lvalue.locToken
                    );
            }

            if (this.checkToken(TokenType.ASSIGNMENT_TKN)) {
                this.err
                    .atToken("Error in type constructor", typeConstruct.locToken)
                    .atToken_PANIC("Assignment in a type constructor must use <- instead of =", this.peekToken());
            }
            this.assertToken(
                TokenType.REVERSE_ARROW_TKN,
                "<-",
                "Assignment in a type constructor must assign a value using <-"
            );
            this.nextToken(); //Consume <-
            declaration.rvalue = this.parseExpression();
            typeConstruct.assignments.push(<AST.ASTSingleVarDeclaration>declaration);
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
        this.assertToken(TokenType.BACKSLASH_TKN, "\\", "Function type must start with a backslash");
        this.nextToken();

        let type = new AST.ASTFunctionType(this.peekToken());
        this.nextToken();
        type.inputType = [];
        while (!this.checkToken(TokenType.RIGHT_PARENS_TKN) && !this.checkToken(TokenType.END_TKN)) {
            type.inputType.push(this.parseType());
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
                .atToken("Error in type specifier", type.locToken)
                .atAfterLastToken_PANIC("Unterminated code at end of file. Expected another type", this.peekToken());
        } else {
            this.nextToken(); //Consume right parenthesis
        }

        this.assertToken(TokenType.ARROW_TKN, "->", "Function type declarations must declare a return type.");
        this.nextToken(); //Consume -> function
        type.outType = this.parseType();
        return type;
    }
    private parseLambda(): AST.ASTLambda {
        this.assertToken(TokenType.BACKSLASH_TKN, "\\", "Lambda expression must start with \\");
        let lambda = new AST.ASTLambda(this.nextToken());

        lambda.parameters = [];
        while (!this.checkToken(TokenType.END_TKN)) {
            let param = new AST.ASTName(this.peekToken());
            this.assertToken(
                TokenType.IDENTIFIER_TKN,
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
                .atToken("Error in lambda expression", lambda.locToken)
                .atAfterLastToken_PANIC(
                    "Unterminated code at end of file. Expected another parameter",
                    this.peekToken()
                );
        }
        this.assertToken(TokenType.CONST_ASSIGNMENT_TKN, "=>", "Lambdas require => after the parameters");
        this.nextToken(); // Consume =>
        if (this.checkToken(TokenType.RETURN_TKN)) {
            this.err.atToken_PANIC("Return keyword is not accepted in a lambda definition", this.peekToken());
        }
        lambda.expression = this.parseExpression();

        return lambda;
    }
}
