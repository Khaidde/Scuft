export function err(msg: string, line: number) {
    throw new Error(msg + " at line " + line);
};