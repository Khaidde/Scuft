export function err(msg: string, line: number, col: number) {
    return new Error("(line:" + line + ",col:" + col + "): " + msg);
}
