import Lexer from "./Lexer";
import Parser from "./Parser";

import simpleTest from "../test/1-SimpleTest.scft";
import expressionTest from "../test/4-ExpressionTest.scft";
import moduleTypeDefTest from "../test/5-ModuleTypeDefinitionTest.scft";
import declareTypeTest from "../test/6-DeclarationTypeInfer.scft";

import { printAST } from "./Ast";
import { printScope } from "./Scope";
import TypeChecker from "./TypeChecker";
import { Scope } from "./Scope";
import { ErrorHandler } from "./ErrorHandler";
export default function () {
    function testString(input: string) {
        // let lexer = new Lexer(input);
        // console.log(lexer.lex());

        let errHandler = ErrorHandler.fromSource("", input);
        let parser = new Parser(new Lexer(input, errHandler), errHandler);
        let prgm = parser.parseProgram();

        printAST(prgm);

        // let typeCheck = new TypeChecker(parser.globalScope, errHandler);
        // typeCheck.typeInferProgram(prgm);
        printScope(parser.globalScope);

        console.log("---------------------");
    }
    // testString(simpleTest);
    // testString(literalTest);
    // testString(typeTest);
    // testString(expressionTest);
    testString(moduleTypeDefTest);

    // testString(declareTypeTest);

    // let parser = new Parser(new Lexer(declareTypeTest));
    // let prgm = parser.parseProgram();
    // printAST(prgm);
    // let typeCheck = new TypeChecker();
    // typeCheck.typeInferProgram(prgm);
    // printScope(typeCheck.globalScope);

    // testString("b2 = (a: num, b: num) -> num {};");
}
