import Lexer from "./Lexer";
import Parser from "./Parser";

import simpleTest from "../test/1-SimpleTest.scft";
import literalTest from "../test/2-LiteralTest.scft";
import typeTest from "../test/3-TypeTest.scft";

import testFile from "./TestFile.scft";

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
    testString(simpleTest);
    testString(literalTest);
    testString(typeTest);
    testString(testFile);

    // testString("b2 = (a: num, b: num) -> num {};");
}
