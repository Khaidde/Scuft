import Lexer from "./Lexer";

import { ASTExpression, ASTDeclaration, ASTStatement, ASTProgram, ASTType } from "./Ast";
import TokenType from "./TokenType";
import { err } from "../ErrorHandling";

export default class Parser {
    private lexer: Lexer;
    constructor(lexer: Lexer) {
        this.lexer = lexer;
    }
    private assertToken(type: TokenType): boolean {
        return this.lexer.peekToken().type === type;
    }
    parseProgram(): ASTProgram {
        let prgmAST = new ASTProgram();
        while (this.lexer.peekToken().type != TokenType.END_TKN) {
            //TODO check other types of nodes
            prgmAST.statements.push(this.parseStatement());
        }
        return prgmAST;
    }
    private parseStatement(): ASTStatement {
        let token = this.lexer.peekToken();

        let statement;
        switch (token.type) {
            case TokenType.IDENTIFIER_TKN: // a : int
                // TODO have to check next token generally
                statement = this.parseDeclaration();
                break;
            default:
                throw err("Invalid statement: " + token.stringValue, token.line, token.col);
        }
        if (!this.assertToken(TokenType.SEMI_COLON_TKN)) {
            let tkn = this.lexer.nextToken();
            throw err("No semi-colon at statement end", tkn.line, tkn.col);
        } else {
            this.lexer.nextToken();
        }
        return statement;
    }
    private parseDeclaration(): ASTDeclaration {
        let declaration = new ASTDeclaration();
        declaration.lvalue = this.lexer.nextToken();

        if (this.assertToken(TokenType.COLON_TKN)) {
            this.lexer.nextToken();
            declaration.type = this.parseType();
        }
        if (this.assertToken(TokenType.ASSIGNMENT_TKN)) {
            this.lexer.nextToken();
            declaration.rvalue = this.parseExpression();
        }
        return declaration;
    }
    private parseType(): ASTType {
        let astType = new ASTType();
        if (this.assertToken(TokenType.LEFT_PARENS_TKN)) {
            this.lexer.nextToken();
            astType.inputType = [];
            while (!this.assertToken(TokenType.RIGHT_PARENS_TKN)) {
                if (!this.assertToken(TokenType.COMMA_TKN)) {
                    astType.inputType.push(this.parseDeclaration());
                } else {
                    this.lexer.nextToken();
                }
            }
            this.lexer.nextToken();
            if (this.assertToken(TokenType.FUNC_MAPPING_TKN)) {
                this.lexer.nextToken();
                astType.outType = this.parseType();
            }
        } else {
            astType.baseType = this.lexer.nextToken();
        }
        return astType;
    }
    private parseExpression(): ASTExpression {
        let token = this.lexer.peekToken();
        switch (token.type) {
            case TokenType.NUMERIC_LITERAL_TKN:
                let literal = new ASTExpression();
                literal.value = this.lexer.nextToken();
                return literal;
            default:
                throw err("Invalid expression " + token.stringValue, token.line, token.col);
        }
    }
}
