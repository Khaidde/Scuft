import Lexer from "./Lexer";
import Parser from "./Parser";

import simpleTest from "../test/1-SimpleTest.scft";
import expressionTest from "../test/4-ExpressionTest.scft";
import moduleTypeDefTest from "../test/5-ModuleTypeDefinitionTest.scft";

import * as ASTDebug from "./Ast";
export default function () {
    function testString(input: string) {
        // let lexer = new Lexer(input);
        // console.log(lexer.lex());

        let parser = new Parser(new Lexer(input));
        let prgm = parser.parseProgram();
        console.log(prgm);

        ASTDebug.printAST(prgm);

        console.log("---------------------");
    }
    // testString(simpleTest);
    // testString(literalTest);
    // testString(typeTest);
    //testString(expressionTest);
    testString(moduleTypeDefTest);

    // testString("b2 = (a: num, b: num) -> num {};");
}
