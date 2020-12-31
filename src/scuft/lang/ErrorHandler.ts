import { Token } from "./Token";
import Lexer from "./Lexer";

export class ErrorHandler {
    static fromHandler(header: string, errorHandler: ErrorHandler): ErrorHandler {
        let handler = new ErrorHandler(header);
        handler.lines = errorHandler.lines;
        return handler;
    }
    static fromSource(header: string, sourceCode: string): ErrorHandler {
        let handler = new ErrorHandler(header);
        let begin = 0;
        let lineCount = 1;
        for (let i = 0; i < sourceCode.length; i++) {
            if (sourceCode.charAt(i) === "\n") {
                handler.lines[lineCount++] = sourceCode.slice(begin, i);
                begin = i + 1;
            }
        }
        handler.lines[lineCount] = sourceCode.slice(begin, sourceCode.length);
        return handler;
    }
    private static readonly TAB_WIDTH = 4;
    private header: string;
    private lines: string[] = [];
    private constructor(header: string) {
        this.header = header;
    }
    private errorQueue: string[] = [];
    panic(): never {
        let err = this.header;
        for (let i = 0; i < this.errorQueue.length; i++) {
            err += this.errorQueue[i];
        }
        throw err;
    }
    warn() {
        let err = this.header;
        for (let i = 0; i < this.errorQueue.length; i++) {
            err += this.errorQueue[i];
        }
        console.warn(err);
        this.errorQueue = [];
    }
    private errorHeader(msg: string, line: number, c?: number): string {
        let optionalCol = "";
        if (c !== undefined) {
            let str = this.lines[line].slice(0, c);
            let col = 0;
            for (let i = 0; i < str.length; i++) {
                if (str.charAt(i) === "\t") {
                    col += ErrorHandler.TAB_WIDTH;
                } else {
                    col++;
                }
            }
            optionalCol = ", col:" + (col + 1);
        }
        return "(line:" + line + optionalCol + "): " + msg + "\n";
    }
    private formatLine(leftIndentSpace: number, line: number, fileLine: string): string {
        if (line < 1) return "";
        let strNumber = line + "";
        for (let i = 0; i < leftIndentSpace - strNumber.length; i++) {
            strNumber = " " + strNumber;
        }
        return "\t" + strNumber + "\t" + fileLine + "\n";
    }
    private getLineFromSource(line: number): string {
        return this.lines[line].trimLeft().split("\t").join(" ");
    }
    private makeIndicator(line: number, c0: number, c1: number): string {
        console.assert(c0 >= 0, "c0=%s must be non-negative", c0);
        console.assert(c1 > c0, "c1=%s must be larger than c0=%s", c1, c0);
        let indentLen = this.lines[line].length - this.lines[line].trimLeft().length;
        return " ".repeat(c0 - indentLen) + "^".repeat(c1 - c0);
    }
    insert(msg: string): ErrorHandler {
        this.errorQueue.push(msg);
        return this;
    }
    atPoint(msg: string, line: number, c: number): ErrorHandler {
        let error = this.errorHeader(msg, line, c);
        let len = Math.floor(Math.log10(line + 1)) + 1;
        error += this.formatLine(len, line - 1, "");
        error += this.formatLine(len, line, this.getLineFromSource(line));
        error += this.formatLine(len, line + 1, this.makeIndicator(line, c, c + 1));
        this.errorQueue.push(error);
        return this;
    }
    atPoint_PANIC(msg: string, line: number, c: number): never {
        this.atPoint(msg, line, c);
        this.panic();
    }
    atToken(msg: string, token: Token): ErrorHandler {
        return this.atPoint(msg, token.line, token.c);
    }
    atToken_PANIC(msg: string, token: Token): never {
        this.atPoint(msg, token.line, token.c);
        this.panic();
    }
    atAfterLastToken(msg: string, curToken: Token): ErrorHandler {
        let curLine = curToken.line;
        let curLineStr = this.lines[curLine];
        let cur = curToken.c;
        do {
            if (cur <= 0 && curLine > 1) {
                curLine--;
                curLineStr = this.lines[curLine];
                cur = curLineStr.length;
            }
            cur--;
        } while ((cur > 0 || curLine >= 1) && Lexer.isWhitespace(curLineStr.charAt(cur)));
        return this.atPoint(msg, curLine, cur + 1);
    }
    atAfterLastToken_PANIC(msg: string, curToken: Token): never {
        this.atAfterLastToken(msg, curToken);
        this.panic();
    }
}
