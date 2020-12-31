const DEFAULT_FMT = "color: #777;";

export function applyFmt(str: string, fmt: string[], format = DEFAULT_FMT): string {
    fmt.push(format);
    fmt.push(DEFAULT_FMT);
    return "%c" + str + "%c";
}

export function merge(firstStr: string[], secondStr: string[]): string[] {
    for (let i = 0; i < secondStr.length; i++) {
        firstStr.push(secondStr[i]);
    }
    return firstStr;
}

export function space(amount: number, strA: string[]): string[] {
    const prefix = " ".repeat(amount);
    for (let i = 0; i < strA.length; i++) {
        strA[i] = prefix + strA[i];
    }
    return strA;
}

export function tab(strA: string[]): string[] {
    for (let i = 0; i < strA.length; i++) {
        strA[i] = "    " + strA[i];
    }
    return strA;
}

export function bar(strA: string[]): string[] {
    for (let i = 0; i < strA.length; i++) {
        strA[i] = " |  " + strA[i];
    }
    return strA;
}
