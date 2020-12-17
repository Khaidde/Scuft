import Lexer from "./Lexer";
import Parser from "./Parser";
import TokenType from "./TokenType";

export default function () {
    let input = "testVar: num = 345.6;\n a1=3; cats: string;";

    let l: Lexer = new Lexer(input);
    console.log(l.lex());

    let p: Parser = new Parser(new Lexer(input));
    console.log(p.parseProgram());

    console.log("---------------------");

    let input2 = "b2: (a:num, b:num) -> num = 3;";
    console.log(new Parser(new Lexer(input2)).parseProgram());
}
