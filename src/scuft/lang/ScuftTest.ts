import Lexer from "./Lexer";
import Parser from "./Parser";
import TypeChecker from "./TypeChecker";

import simpleTest from "../test/1-SimpleTest.scft";
import literalTest from "../test/2-LiteralTest.scft";
import expressionTest from "../test/4-ExpressionTest.scft";
import moduleTypeDefTest from "../test/5-ModuleTypeDefinitionTest.scft";
import declareTypeTest from "../test/6-DeclarationTypeInfer.scft";

import { printAST } from "./Ast";
import { ErrorHandler } from "./ErrorHandler";

export default function () {
    function testString(input: string) {
        // let lexer = new Lexer(input);
        // console.log(lexer.lex());

        let errHandler = ErrorHandler.fromSource("", input);
        let parser = new Parser(new Lexer(input, errHandler), errHandler);
        let prgm = parser.parseProgram();

        printAST(prgm);

        let typeCheck = new TypeChecker(parser.globalScope, errHandler);
        typeCheck.typeCheckProgram(prgm);
        parser.globalScope.printScope();

        printAST(prgm);

        console.log("---------------------");
    }
    // testString(simpleTest);
    // testString(literalTest);
    // testString(typeTest);
    // testString(expressionTest);
    // testString(moduleTypeDefTest);
    testString(declareTypeTest);

    // testString("b2 = (a: num, b: num) -> num {};");
}
