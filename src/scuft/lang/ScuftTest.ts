import Lexer from "./Lexer";
import Parser from "./Parser";
import TypeChecker from "./TypeChecker";
import Interpreter from "./Interpreter";

import simpleTest from "../test/1-SimpleTest.scft";
import literalTest from "../test/2-LiteralTest.scft";
import expressionTest from "../test/4-ExpressionTest.scft";
import moduleTypeDefTest from "../test/5-ModuleTypeDefinitionTest.scft";
import declareTypeTest from "../test/6-DeclarationTypeInfer.scft";
import interpTest from "../test/7-InterpTest.scft";

import { printAST } from "./Ast";
import { printScope } from "./Scope";
import { ErrorHandler } from "./ErrorHandler";

export default function () {
    function testString(input: string) {
        let errHandler = ErrorHandler.fromSource("", input);

        // let lexer = new Lexer(input, errHandler);
        // console.log(lexer.lex());

        let parser = new Parser(new Lexer(input, errHandler), errHandler);
        let prgm = parser.parseProgram();

        printAST(prgm);

        let typeCheck = new TypeChecker(errHandler);
        typeCheck.typeCheckProgram(prgm);
        printScope(prgm.scope);

        // let interp = new Interpreter(errHandler);
        // interp.interpProgram(prgm);
    }
    // testString(simpleTest);
    // testString(literalTest);
    // testString(typeTest);
    // testString(expressionTest);
    // testString(moduleTypeDefTest);
    // testString(declareTypeTest);
    testString(interpTest);

    // testString("b2 = (a: num, b: num) -> num {};");
}
