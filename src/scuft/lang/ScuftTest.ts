import Lexer from "./Lexer"
import TokenType from "./TokenType"

export default function() {

    let input = "testVar : num = 345.6=;\n"
                + "a1 = 3;";

    let l: Lexer = new Lexer(input);
    console.log(l.lex());

    console.log("---------------------");

    let input2 = "num5    =       ~342`009*93844234090w1+9034 ";
    let l2 = new Lexer(input2);
    console.log(l2.lex());
}