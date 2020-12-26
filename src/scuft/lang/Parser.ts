import Lexer from "./Lexer";

import * as AST from "./Ast";
import Token from "./Token";
import TokenType from "./TokenType";

export default class Parser {
    private lexer: Lexer;
    constructor(lexer: Lexer) {
        this.lexer = lexer;

        this.initPrecedenceMap();
    }
    private readonly ERROR_HEADER = "----ParseError----\n";
    private err(msg: string, token: Token): never {
        throw this.ERROR_HEADER + this.lexer.errHandler.newErrPoint(msg, token.line, token.c);
    }
    private errAfterLast(msg: string, curToken: Token): never {
        throw this.ERROR_HEADER + this.lexer.errHandler.newErrAfterLastToken(msg, curToken);
    }
    private checkToken(type: TokenType): boolean {
        return this.peekToken().type === type;
    }
    private assertToken(type: TokenType, expectedTkn?: string, msg?: string) {
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
            this.errAfterLast(res, tkn);
        }
    }
    private tokenQueue: Token[] = [];
    private matchAheadTokens(...tokenTypes: TokenType[]): boolean {
        let res = true;
        for (let i = 0; i < tokenTypes.length; i++) {
            if (tokenTypes[i] !== this.lookAheadToken(i + 1).type) {
                res = false;
                break;
            }
        }
        return res;
    }
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
    private nextToken(): Token {
        if (this.tokenQueue.length > 0) {
            return this.tokenQueue.shift()!;
        } else {
            return this.lexer.nextToken();
        }
    }
    parseProgram(): AST.ASTProgram {
        let program = new AST.ASTProgram();
        program.modules = [];
        program.typeDefinitions = [];
        program.statements = [];
        while (this.peekToken().type != TokenType.END_TKN) {
            switch (this.peekToken().type) {
                case TokenType.MODULE_TKN:
                    program.modules.push(this.parseModule());
                    break;
                case TokenType.TYPE_TKN:
                    program.typeDefinitions.push(this.parseTypeDefinition());
                    break;
                case TokenType.WITH_TKN:
                    this.err("Can't use with statement in global scope", this.peekToken());
                default:
                    program.statements.push(this.parseStatement());
            }
        }
        return program;
    }
    private parseModule(): AST.ASTModule {
        let module = new AST.ASTModule();
        this.nextToken();
        this.assertToken(TokenType.IDENTIFIER_TKN, "a module name", "Module must declare name after module keyword");
        module.name = this.nextToken();

        module.typeDefinitions = [];
        module.statements = [];
        this.assertToken(TokenType.LEFT_CURLY_TKN, "{");
        this.nextToken();
        let tkn = this.peekToken();
        while (tkn.type !== TokenType.RIGHT_CURLY_TKN) {
            switch (tkn.type) {
                case TokenType.TYPE_TKN:
                    module.typeDefinitions.push(this.parseTypeDefinition());
                    break;
                case TokenType.MODULE_TKN:
                    this.err("Module declaration must be declared in global scope", tkn);
                default:
                    module.statements.push(this.parseStatement());
                    break;
            }
            tkn = this.peekToken();
        }
        this.assertToken(TokenType.RIGHT_CURLY_TKN, "}");
        this.nextToken();
        return module;
    }
    private parseTypeDefinition(): AST.ASTTypeDefinition {
        let typeDef = new AST.ASTTypeDefinition();
        this.nextToken();
        this.assertToken(
            TokenType.IDENTIFIER_TKN,
            "identifier",
            "Type definition must declare name after type keyword"
        );
        typeDef.name = this.nextToken();

        typeDef.typeDeclarations = [];
        this.assertToken(TokenType.LEFT_CURLY_TKN, "{");
        this.nextToken();
        while (!this.checkToken(TokenType.RIGHT_CURLY_TKN)) {
            typeDef.typeDeclarations.push(this.parseDeclaration());
            this.assertToken(TokenType.SEMI_COLON_TKN, ";", "Semi-colons are required at the end of statements");
            this.nextToken();
        }
        this.assertToken(TokenType.RIGHT_CURLY_TKN, "}");
        this.nextToken();
        return typeDef;
    }
    private parseBlock(): AST.ASTBlock {
        let block = new AST.ASTBlock();
        block.statements = [];
        block.withModules = [];
        this.assertToken(TokenType.LEFT_CURLY_TKN, "{");
        this.nextToken();
        let tkn = this.peekToken();
        while (!this.checkToken(TokenType.RIGHT_CURLY_TKN)) {
            switch (tkn.type) {
                case TokenType.TYPE_TKN:
                    this.err("Type declarations must be declared in a module or with global scope", tkn);
                case TokenType.MODULE_TKN:
                    this.err("Module declaration must be declared in global scope", tkn);
                case TokenType.WITH_TKN:
                    this.nextToken(); //consume with token;
                    this.assertToken(
                        TokenType.IDENTIFIER_TKN,
                        "a module name",
                        "With keyword must be followed by a name"
                    );
                    block.withModules.push(this.nextToken());
                    this.assertToken(
                        TokenType.SEMI_COLON_TKN,
                        ";",
                        "Semi-colons are required at the end of with statements"
                    );
                    this.nextToken();
                    break;
                default:
                    block.statements.push(this.parseStatement());
                    break;
            }
            tkn = this.peekToken();
        }
        this.nextToken(); //Right curly
        return block;
    }
    private parseStatement(): AST.ASTStatement {
        let tkn = this.peekToken();

        let statement;
        switch (tkn.type) {
            case TokenType.IDENTIFIER_TKN:
                if (this.lookAheadToken(this.getEndOfMember()).type === TokenType.LEFT_PARENS_TKN) {
                    statement = this.parseCall();
                    this.assertToken(
                        TokenType.SEMI_COLON_TKN,
                        ";",
                        "Semi-colons are required at the end of function calls"
                    );
                    this.nextToken();
                } else {
                    statement = this.parseDeclaration();
                    if (statement.rvalue && statement.rvalue.constructor.name === AST.ASTFunction.name) {
                        if (this.checkToken(TokenType.SEMI_COLON_TKN)) {
                            let funcName = this.lexer.errHandler.newErrToken(
                                "Error in declared function",
                                statement.lvalue.rootName
                            );
                            let semiColonError = this.lexer.errHandler.newErrAfterLastToken(
                                "Semi-colons are not needed after a function declaration",
                                this.peekToken()
                            );
                            throw this.ERROR_HEADER + funcName + semiColonError;
                        } else {
                            break; //Avoid asserting the semi-colon by breaking out
                        }
                    }
                    if (this.checkToken(TokenType.RIGHT_PARENS_TKN)) {
                        this.err("Unbalanced parenthesis. Expected a semi-colon", this.peekToken());
                    }
                    this.assertToken(
                        TokenType.SEMI_COLON_TKN,
                        ";",
                        "Semi-colons are required at the end of declarations"
                    );
                    this.nextToken();
                }
                break;
            case TokenType.IF_TKN:
                statement = this.parseIf();
                break;
            case TokenType.WHILE_TKN:
                statement = this.parseWhile();
                break;
            case TokenType.FOR_TKN:
                statement = this.parseFor();
                break;
            case TokenType.BREAK_TKN:
                statement = new AST.ASTBreak();
                this.nextToken();
                this.assertToken(
                    TokenType.SEMI_COLON_TKN,
                    ";",
                    "Semi-colons are required at the end of break statements"
                );
                this.nextToken();
                break;
            case TokenType.CONTINUE_TKN:
                statement = new AST.ASTContinue();
                this.nextToken();
                this.assertToken(
                    TokenType.SEMI_COLON_TKN,
                    ";",
                    "Semi-colons are required at the end of continue statements"
                );
                this.nextToken();
                break;
            case TokenType.RETURN_TKN:
                statement = this.parseReturn();
                break;
            default:
                this.err("Statement can't start with " + tkn.stringValue, tkn);
        }
        return statement;
    }
    private parseDeclaration(): AST.ASTDeclaration {
        let declaration = new AST.ASTDeclaration();

        this.assertToken(TokenType.IDENTIFIER_TKN, "a variable reference name", "Declaration must have a name");
        declaration.lvalue = this.parseMember();

        if (this.checkToken(TokenType.COLON_TKN)) {
            this.nextToken();
            declaration.type = this.parseType();
        }
        if (this.checkToken(TokenType.ASSIGNMENT_TKN)) {
            this.nextToken();

            //Distinguish function declaration from variable declaration
            let functionCheckAhead = 1;
            let aheadToken = this.lookAheadToken(functionCheckAhead);
            while (
                aheadToken.type !== TokenType.END_TKN &&
                aheadToken.type !== TokenType.COLON_TKN &&
                aheadToken.type !== TokenType.MAPPING_TKN &&
                !(
                    aheadToken.type === TokenType.RIGHT_PARENS_TKN &&
                    this.lookAheadToken(functionCheckAhead + 1).type === TokenType.LEFT_CURLY_TKN
                ) &&
                aheadToken.type !== TokenType.SEMI_COLON_TKN
            ) {
                functionCheckAhead++;
                aheadToken = this.lookAheadToken(functionCheckAhead);
            }
            if (aheadToken.type === TokenType.END_TKN) {
                this.err("Unfinished declaration at the end of the file", declaration.lvalue.rootName);
            } else if (aheadToken.type === TokenType.SEMI_COLON_TKN) {
                declaration.rvalue = this.parseExpression(this.LOWEST_PRECEDENCE, 0);
            } else {
                declaration.rvalue = this.parseFunction();
            }
        }
        if (!declaration.type && !declaration.rvalue) {
            if (this.checkToken(TokenType.SEMI_COLON_TKN)) {
                // randomVarName;
                // ^
                this.err("Expressions can't serve as statements in this language", declaration.lvalue.rootName);
            } else {
                // a for
                //   ^
                this.err(
                    this.peekToken().stringValue +
                        " is not allowed here. Declaration must either assign a value using = or specify a type using :",
                    this.peekToken()
                );
            }
        }

        return declaration;
    }
    private parseType(): AST.ASTType {
        let astType = new AST.ASTType();
        if (this.checkToken(TokenType.LEFT_PARENS_TKN)) {
            this.nextToken();
            astType.inputType = [];
            while (!this.checkToken(TokenType.RIGHT_PARENS_TKN) && !this.checkToken(TokenType.END_TKN)) {
                astType.inputType.push(this.parseType());
                if (this.checkToken(TokenType.COMMA_TKN)) {
                    this.nextToken();
                } else if (!this.checkToken(TokenType.RIGHT_PARENS_TKN)) {
                    // a: (num; )
                    //        ^
                    this.errAfterLast("Invalid type definition. Expected either a comma or )", this.peekToken());
                }
            }
            if (this.checkToken(TokenType.END_TKN)) {
                // a : (num, num
                this.errAfterLast("Unterminated type declaration at end of file", this.peekToken());
            }
            this.nextToken();

            this.assertToken(TokenType.MAPPING_TKN, "->", "Function type declarations must declare a return type.");
            this.nextToken();
            astType.outType = this.parseType();
        } else {
            let tkn = this.peekToken();
            switch (tkn.type) {
                case TokenType.NUM_TKN:
                case TokenType.STRING_TKN:
                case TokenType.BOOL_TKN:
                case TokenType.VOID_TKN:
                    astType.type = tkn;
                    this.nextToken();
                    break;
                case TokenType.IDENTIFIER_TKN:
                    astType.type = this.parseMember();
                    break;
                case TokenType.END_TKN:
                    //a: (num, num) ->    [End of File]
                    //                ^
                    this.errAfterLast("Unterminated type declaration at end of file", tkn);
                default:
                    // a: 3 = 3
                    //    ^
                    this.err("Type must be an identifier", tkn);
            }
        }
        return astType;
    }
    private parseIf(): AST.ASTIf {
        let ifStatement = new AST.ASTIf();
        this.assertToken(TokenType.IF_TKN, "If statement must start with an if token");
        this.nextToken(); //Consume if token
        ifStatement.condition = this.parseExpression(this.LOWEST_PRECEDENCE, 0);
        ifStatement.consequence = this.parseBlock();
        if (this.checkToken(TokenType.ELSE_TKN)) {
            this.nextToken(); //Consume else token
            if (this.checkToken(TokenType.IF_TKN)) {
                ifStatement.alternative = this.parseIf();
            } else if (this.checkToken(TokenType.LEFT_CURLY_TKN)) {
                ifStatement.alternative = this.parseBlock();
            } else {
                this.err(this.peekToken().stringValue + " is not allowed here", this.peekToken());
            }
        }
        return ifStatement;
    }
    private parseWhile(): AST.ASTWhile {
        let whileStatement = new AST.ASTWhile();
        this.assertToken(TokenType.WHILE_TKN, "While statement must start with a while token");
        this.nextToken(); //Consume while token
        whileStatement.condition = this.parseExpression(this.LOWEST_PRECEDENCE, 0);
        whileStatement.block = this.parseBlock();
        return whileStatement;
    }
    private parseFor(): AST.ASTFor {
        let forStatement = new AST.ASTFor();
        this.assertToken(TokenType.FOR_TKN, "For statement must start with a for token");
        this.nextToken(); //Consume for token

        if (this.checkToken(TokenType.LEFT_PARENS_TKN)) {
            this.err("For loops don't require encapsulating parenthesis in this language", this.peekToken());
        }

        switch (this.lookAheadToken(2).type) {
            case TokenType.LEFT_CURLY_TKN:
                this.assertToken(TokenType.IDENTIFIER_TKN, "an iterable");
                forStatement.iterableName = this.parseMember();
                break;
            case TokenType.COMMA_TKN:
                this.assertToken(TokenType.IDENTIFIER_TKN, "an item variable declarations");
                forStatement.itemParamDec = this.nextToken();
                this.nextToken(); //Consume comma
                this.assertToken(TokenType.IDENTIFIER_TKN, "an index variable declaration");
                forStatement.indexParamDec = this.nextToken();
                this.assertToken(TokenType.IN_TKN);
                this.nextToken(); //Consume in
                this.assertToken(TokenType.IDENTIFIER_TKN, "an iterable");
                forStatement.iterableName = this.parseMember();
                break;
            case TokenType.IN_TKN:
                this.assertToken(TokenType.IDENTIFIER_TKN, "an index variable declaration");
                forStatement.itemParamDec = this.nextToken();
                this.nextToken(); //Consume in
                if (this.checkToken(TokenType.IDENTIFIER_TKN)) {
                    forStatement.iterableName = this.parseMember();
                    break;
                } else {
                    let lowerBound = this.lookAheadToken(2);
                    if (lowerBound.type !== TokenType.NUMERIC_LITERAL_TKN) {
                        this.err("Expected a number for the lower bound", lowerBound);
                    }
                    if (this.checkToken(TokenType.LEFT_SQUARE_TKN)) {
                        forStatement.lowerBound = <number>lowerBound.value;
                    } else if (this.checkToken(TokenType.LEFT_PARENS_TKN)) {
                        forStatement.lowerBound = <number>lowerBound.value + 1;
                    } else {
                        this.err("Expected either a [ or a (", this.peekToken());
                    }
                    this.nextToken(); //consume ( or [
                    this.nextToken(); //consume the number
                    this.assertToken(TokenType.COMMA_TKN, "Invalid for loop range");
                    this.nextToken(); //Consume comma
                    let upperBound = this.nextToken();
                    if (upperBound.type !== TokenType.NUMERIC_LITERAL_TKN) {
                        this.err("Expected a number for the upper bound", upperBound);
                    }
                    if (this.checkToken(TokenType.RIGHT_SQUARE_TKN)) {
                        forStatement.upperBound = <number>upperBound.value;
                    } else if (this.checkToken(TokenType.RIGHT_PARENS_TKN)) {
                        forStatement.upperBound = <number>upperBound.value - 1;
                    } else {
                        this.err("Expected either a ] or a )", this.peekToken());
                    }
                    this.nextToken(); //consume ) or ]
                    break;
                }
            default:
                this.err("Invalid for loop", this.peekToken());
        }
        forStatement.block = this.parseBlock();
        return forStatement;
    }
    private parseReturn(): AST.ASTReturn {
        let returnStatement = new AST.ASTReturn();
        this.assertToken(TokenType.RETURN_TKN, "Return statement must start with a return token");
        this.nextToken(); //Consume the return token

        if (this.peekToken().type !== TokenType.SEMI_COLON_TKN) {
            returnStatement.returnValue = this.parseExpression(this.LOWEST_PRECEDENCE, 0);
        }
        this.assertToken(TokenType.SEMI_COLON_TKN, ";", "Semi-colons are required at the end of return statements");
        this.nextToken();
        return returnStatement;
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
        this.setPrecedence(11, TokenType.OP_MULT_TKN, TokenType.OP_DIVIDE_TKN);
        this.setPrecedence(12, TokenType.OP_CARROT_TKN);
    }
    private parseExpression(precedence: number, depthCount: number): AST.ASTExpression {
        let expression!: AST.ASTExpression;
        let tkn = this.peekToken();
        while (
            tkn.type !== TokenType.SEMI_COLON_TKN &&
            tkn.type !== TokenType.COMMA_TKN &&
            tkn.type !== TokenType.RIGHT_PARENS_TKN &&
            tkn.type !== TokenType.RIGHT_CURLY_TKN &&
            tkn.type !== TokenType.END_TKN
        ) {
            switch (tkn.type) {
                case TokenType.LEFT_PARENS_TKN:
                    // a = 3 ( ...
                    //       ^
                    if (expression) this.err("Left parenthesis in an expression can only follow an operator", tkn);
                    this.nextToken(); //Consume left parenthesis
                    expression = this.parseExpression(this.LOWEST_PRECEDENCE, depthCount + 1);
                    this.nextToken(); //Consume right parenthesis
                    break;
                case TokenType.NUMERIC_LITERAL_TKN:
                    // a = 5 6 ...
                    //       ^
                    if (expression) this.err("Number in an expression can only follow an operator", tkn);
                    expression = new AST.ASTLiteral();
                    expression.value = this.nextToken();
                    break;
                case TokenType.STRING_LITERAL_TKN:
                    // a = 5 "this is a string" ...
                    //       ^
                    if (expression) this.err("String in an expression can only follow an operator", tkn);
                    expression = new AST.ASTLiteral();
                    expression.value = this.nextToken();
                    break;
                case TokenType.COND_TRUE_TKN:
                    // a = 5 true ...
                    //       ^
                    if (expression) this.err("TRUE in an expression can only follow an operator", tkn);
                    expression = new AST.ASTLiteral();
                    expression.value = this.nextToken();
                    break;
                case TokenType.COND_FALSE_TKN:
                    // a = 5 false ...
                    //       ^
                    if (expression) this.err("FALSE in an expression can only follow an operator", tkn);
                    expression = new AST.ASTLiteral();
                    expression.value = this.nextToken();
                    break;
                case TokenType.COND_NOT_TKN:
                    // a = 5 ! true ...
                    //       ^
                    if (expression) this.err("NOT (!) in an expression can only follow a boolean expression", tkn);
                    expression = new AST.ASTUnaryOperator();
                    expression.operation = this.nextToken();
                    expression.value = this.parseExpression(precedence, depthCount);
                    break;
                case TokenType.BIN_NOT_TKN:
                    // a = 5 ~(3 + 4) ...
                    //       ^
                    if (expression) this.err("NOT (~) in an expression can only follow a numeric literal", tkn);
                    expression = new AST.ASTUnaryOperator();
                    expression.operation = this.nextToken();
                    expression.value = this.parseExpression(precedence, depthCount);
                    break;
                case TokenType.IDENTIFIER_TKN:
                    switch (this.lookAheadToken(this.getEndOfMember()).type) {
                        case TokenType.LEFT_PARENS_TKN:
                            // This is a function call
                            // a = 5sin() ...
                            //      ^
                            if (expression) this.err("Function call in an expression can only follow an operator", tkn);
                            expression = this.parseCall();
                            break;
                        case TokenType.LEFT_CURLY_TKN:
                            // This is a type construction
                            // a = 5Point{ x=2, z=3 } ...
                            //      ^
                            if (expression) this.err("Type construction can only follow an operator", tkn);
                            expression = this.parseTypeConstruction();
                            break;
                        default:
                            // This is a variable
                            // a = 5count ...
                            //      ^
                            if (expression) this.err("Variable in an expression can only follow operator", tkn);
                            expression = this.parseMember();
                            break;
                    }
                    break;
                case TokenType.OP_SUBTR_TKN:
                    if (!expression) {
                        expression = new AST.ASTBinaryOperator();
                        expression.lvalue = new AST.ASTLiteral();
                        expression.lvalue.value = new Token("0", tkn.line, tkn.c, TokenType.NUMERIC_LITERAL_TKN);
                        expression.lvalue.value.value = 0;
                        expression.operation = this.nextToken();
                        expression.rvalue = this.parseExpression(this.HIGHEST_PRECEDENCE, depthCount);
                        break;
                    } //Leak to default case (subtraction)
                default:
                    // a = 3 # 4 - 6;
                    //       ^
                    if (!expression) this.err(tkn.stringValue + " is an unknown prefix operator", tkn);
                    let otherPrecedence = this.precedence.get(tkn.type);
                    if (otherPrecedence !== undefined && otherPrecedence > precedence) {
                        let temp = expression;
                        expression = new AST.ASTBinaryOperator();
                        expression.lvalue = temp;
                        expression.operation = this.nextToken();
                        expression.rvalue = this.parseExpression(otherPrecedence, depthCount);
                    } else {
                        return expression;
                    }
            }
            tkn = this.peekToken();
        }
        if (!expression) {
            this.errAfterLast("Expected an expression but got nothing", this.peekToken()); //a = 3 + ;
        } else if (!this.checkToken(TokenType.RIGHT_PARENS_TKN) && depthCount !== 0) {
            this.errAfterLast("Unbalanced parenthesis. Expected " + depthCount + " more )", tkn); //b = (((((0 + 5)
        } else if (this.checkToken(TokenType.END_TKN)) {
            this.errAfterLast("Unterminated expression at end of file", tkn); //a = 3 + 4
        }
        return expression;
    }
    private getEndOfMember(): number {
        console.assert(
            this.checkToken(TokenType.IDENTIFIER_TKN),
            "getEndOfMember should only be used when a member is suspected: the peek token is an identifier"
        );
        let memberCheckAhead = 2;
        while (
            this.lookAheadToken(memberCheckAhead).type == TokenType.DOT_TKN &&
            this.lookAheadToken(memberCheckAhead + 1).type === TokenType.IDENTIFIER_TKN
        ) {
            memberCheckAhead += 2;
        }
        return memberCheckAhead;
    }
    private parseMember(): AST.ASTMember {
        let member = new AST.ASTMember();
        this.assertToken(TokenType.IDENTIFIER_TKN, "a variable reference name");
        member.rootName = this.nextToken();

        if (this.checkToken(TokenType.DOT_TKN)) {
            this.nextToken();
            member.memberSelect = this.parseMember();
        }
        return member;
    }
    private parseTypeConstruction(): AST.ASTTypeConstruction {
        let typeConstruct = new AST.ASTTypeConstruction();
        this.assertToken(TokenType.IDENTIFIER_TKN);
        typeConstruct.typeName = this.parseMember();
        this.nextToken(); // consume left curly token
        typeConstruct.assignments = [];
        while (!this.checkToken(TokenType.RIGHT_CURLY_TKN)) {
            let declaration = this.parseDeclaration();
            if (declaration.type) {
                // p = Particle { a: num = 3 }
                //                ^
                this.err("Type constructor assignment should not specify a type", declaration.lvalue.rootName);
            }
            typeConstruct.assignments.push(declaration);
            if (this.checkToken(TokenType.COMMA_TKN)) {
                let expectedTkn = this.lookAheadToken(2);
                if (expectedTkn.type === TokenType.RIGHT_CURLY_TKN) {
                    // particle = Particle { a = 3,   };
                    //                             ^
                    this.errAfterLast("Expected another assignment after the comma but got nothing", expectedTkn);
                } else {
                    this.nextToken();
                }
            } else if (!this.checkToken(TokenType.RIGHT_CURLY_TKN)) {
                // particle = Particle { a = 3;    b = 4 };
                //                            ^
                this.errAfterLast("Invalid type construction. Expected either a comma or }", this.peekToken());
            }
        }
        this.nextToken();
        return typeConstruct;
    }
    private parseFunction(): AST.ASTFunction {
        let func = new AST.ASTFunction();

        func.paramDeclaration = [];

        this.assertToken(TokenType.LEFT_PARENS_TKN, "(", "Function declaration must specify parameters or use ()");
        this.nextToken();
        while (!this.checkToken(TokenType.RIGHT_PARENS_TKN) && !this.checkToken(TokenType.END_TKN)) {
            func.paramDeclaration.push(this.parseDeclaration());
            if (this.checkToken(TokenType.COMMA_TKN)) {
                let expectedTkn = this.lookAheadToken(2);
                if (expectedTkn.type === TokenType.RIGHT_PARENS_TKN) {
                    // a = (b: num,  )
                    //             ^
                    this.errAfterLast(
                        "Expected another parameter definition after the comma but got nothing",
                        expectedTkn
                    );
                } else {
                    this.nextToken();
                }
            } else if (!this.checkToken(TokenType.RIGHT_PARENS_TKN)) {
                // a = (a:num  b:num)
                //           ^
                this.errAfterLast(
                    "Invalid function parameter definition. Expected either a comma or )",
                    this.peekToken()
                );
            }
        }
        if (this.checkToken(TokenType.END_TKN)) {
            //a = (a:num,     [End of File]
            //           ^
            this.errAfterLast("Unterminated parameter declaration at end of file", this.peekToken());
        }
        this.nextToken();

        if (this.checkToken(TokenType.MAPPING_TKN)) {
            this.nextToken();
            func.returnType = this.parseType();
        }
        if (this.checkToken(TokenType.LEFT_CURLY_TKN)) {
            func.block = this.parseBlock();
        } else {
            //a = (b: num) for { }
            //             ^
            this.err(
                this.peekToken().stringValue +
                    " is not allowed here. Function must either declare a return type using -> or omit completely",
                this.peekToken()
            );
        }

        return func;
    }
    private parseCall(): AST.ASTCall {
        let call = new AST.ASTCall();
        this.assertToken(TokenType.IDENTIFIER_TKN, "a function reference name");
        call.functionName = this.parseMember();

        call.givenParams = [];

        this.assertToken(TokenType.LEFT_PARENS_TKN, "(");
        this.nextToken();
        while (!this.checkToken(TokenType.RIGHT_PARENS_TKN)) {
            if (this.checkToken(TokenType.COMMA_TKN)) {
                let expectedTkn = this.lookAheadToken(2);
                if (expectedTkn.type === TokenType.RIGHT_PARENS_TKN || expectedTkn.type === TokenType.END_TKN) {
                    // a(2, 32,   )
                    //         ^
                    this.errAfterLast("Expected another expression after the comma but got nothing", expectedTkn);
                } else {
                    this.nextToken();
                }
            } else {
                call.givenParams.push(this.parseExpression(this.LOWEST_PRECEDENCE, 0));
            }
        }
        this.nextToken();
        return call;
    }
}
