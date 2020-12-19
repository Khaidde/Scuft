import Lexer from "./Lexer";

import * as AST from "./Ast";
import TokenType from "./TokenType";

export default class Parser {
    private lexer: Lexer;
    constructor(lexer: Lexer) {
        this.lexer = lexer;
    }
    private checkToken(type: TokenType): boolean {
        return this.lexer.peekToken().type === type;
    }
    private assertToken(type: TokenType, expectedTkn?: string, msg?: string) {
        let tkn = this.lexer.peekToken();
        if (tkn.type !== type) {
            let res = "Expected ";
            if (expectedTkn) {
                res += expectedTkn;
            } else {
                res += TokenType[type];
            }
            res += " but got " + tkn.stringValue + " instead";
            if (msg) {
                res += ": " + msg;
            }
            this.lexer.parseErr.errToken(res, tkn);
        }
    }
    parseProgram(): AST.ASTProgram {
        let prgmAST = new AST.ASTProgram();
        while (this.lexer.peekToken().type != TokenType.END_TKN) {
            //TODO check other types of nodes including "types" and "modules"
            switch (this.lexer.peekToken().type) {
                default:
                    prgmAST.statements.push(this.parseStatement());
            }
        }
        return prgmAST;
    }
    private parseBlock(): AST.ASTBlock {
        let block = new AST.ASTBlock();
        this.assertToken(TokenType.LEFT_CURLY_TKN, "{");
        this.lexer.nextToken();
        while (!this.checkToken(TokenType.RIGHT_CURLY_TKN)) {
            block.statements.push(this.parseStatement());
        }
        this.lexer.nextToken(); //Right curly
        return block;
    }
    private parseStatement(): AST.ASTStatement {
        let tkn = this.lexer.peekToken();

        let statement;
        switch (tkn.type) {
            case TokenType.IDENTIFIER_TKN:
                if (this.lexer.lookAheadToken(2).type === TokenType.LEFT_PARENS_TKN) {
                    statement = this.parseCall();
                } else {
                    statement = this.parseDeclaration();
                }
                break;
            default:
                this.lexer.parseErr.errToken("Invalid statement", tkn);
        }
        this.assertToken(TokenType.SEMI_COLON_TKN, ";", "Semi-colons are required at the end of statements");
        this.lexer.nextToken();
        return statement;
    }
    private parseDeclaration(): AST.ASTDeclaration {
        let declaration = new AST.ASTDeclaration();
        declaration.lvalue = this.lexer.nextToken();

        if (this.checkToken(TokenType.COLON_TKN)) {
            this.lexer.nextToken();
            declaration.type = this.parseType();
        }
        if (this.checkToken(TokenType.ASSIGNMENT_TKN)) {
            this.lexer.nextToken();

            if (this.lexer.matchAheadTokens(TokenType.LEFT_PARENS_TKN, TokenType.IDENTIFIER_TKN, TokenType.COLON_TKN)) {
                declaration.rvalue = this.parseFunction();
            } else {
                declaration.rvalue = this.parseExpression();
            }
        }
        return declaration;
    }
    private parseType(): AST.ASTType {
        let astType = new AST.ASTType();
        if (this.checkToken(TokenType.LEFT_PARENS_TKN)) {
            this.lexer.nextToken();
            astType.inputType = [];
            while (!this.checkToken(TokenType.RIGHT_PARENS_TKN)) {
                if (!this.checkToken(TokenType.COMMA_TKN)) {
                    astType.inputType.push(this.parseType());
                } else {
                    this.lexer.nextToken();
                }
            }
            this.lexer.nextToken();

            this.assertToken(TokenType.FUNC_MAPPING_TKN, "->", "Function types must declare a return type");
            this.lexer.nextToken();
            astType.outType = this.parseType();
        } else {
            let tkn = this.lexer.nextToken();
            switch (tkn.type) {
                case TokenType.NUM_TKN:
                case TokenType.STRING_TKN:
                case TokenType.BOOL_TKN:
                case TokenType.VOID_TKN:
                case TokenType.IDENTIFIER_TKN:
                    astType.type = tkn;
                    break;
                default:
                    console.log(this.lexer.lex());
                    console.log(tkn);
                    this.lexer.parseErr.errToken("Invalid type", tkn);
            }
        }
        return astType;
    }
    private parseExpression(): AST.ASTExpression {
        let tkn = this.lexer.peekToken();
        switch (tkn.type) {
            case TokenType.NUMERIC_LITERAL_TKN:
            case TokenType.STRING_LITERAL_TKN:
                let literal = new AST.ASTLiteral();
                literal.value = this.lexer.nextToken();
                return literal;
            default:
                this.lexer.parseErr.errToken("Invalid expression", tkn);
        }
    }
    private parseFunction(): AST.ASTFunction {
        let func = new AST.ASTFunction();

        func.paramDeclaration = [];

        this.assertToken(TokenType.LEFT_PARENS_TKN, "(");
        this.lexer.nextToken();
        while (!this.checkToken(TokenType.RIGHT_PARENS_TKN)) {
            if (!this.checkToken(TokenType.COMMA_TKN)) {
                func.paramDeclaration.push(this.parseDeclaration());
            } else {
                this.lexer.nextToken();
            }
        }
        this.lexer.nextToken();

        if (this.checkToken(TokenType.FUNC_MAPPING_TKN)) {
            this.lexer.nextToken();
            func.returnType = this.parseType();
        }

        func.block = this.parseBlock();

        return func;
    }
    private parseCall(): AST.ASTCall {
        let call = new AST.ASTCall();
        call.functionName = this.lexer.nextToken();

        call.givenParams = [];

        this.assertToken(TokenType.LEFT_PARENS_TKN, "(");
        this.lexer.nextToken();
        while (!this.checkToken(TokenType.RIGHT_PARENS_TKN)) {
            if (!this.checkToken(TokenType.COMMA_TKN)) {
                call.givenParams.push(this.parseExpression());
            } else {
                this.lexer.nextToken();
            }
        }
        this.lexer.nextToken();

        return call;
    }
}
