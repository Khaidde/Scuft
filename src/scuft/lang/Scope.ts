import * as AST from "./Ast";
import { applyFmt, merge, space, tab, bar } from "./Log";

export class Scope {
    static newGlobalScope(): Scope {
        return new Scope();
    }
    static newScopeFrom(parentScope: Scope): Scope {
        let scope = new Scope();
        scope.parent = parentScope;
        if (!parentScope.children) parentScope.children = [];
        parentScope.children.push(scope);
        return scope;
    }
    static newTypeDefScopeFrom(parentScope: Scope): Scope {
        let scope = this.newScopeFrom(parentScope);
        scope.inwardScoping = true;
        return scope;
    }
    table: Map<string, AST.ASTDeclaration> = new Map();
    withModules!: Scope[]; // Store the scope of all "with" modules
    parent!: Scope;
    children!: Scope[];
    inwardScoping: boolean = false;
    private constructor() {}
    // Attempts to add declaration to the scope or spits out an already existing declaration
    attemptAdd(identifierName: string, declaration: AST.ASTDeclaration): AST.ASTDeclaration {
        let otherDec = this.getDeclarationFromName(identifierName);
        if (otherDec) return otherDec;
        this.table.set(identifierName, declaration);
        return declaration;
    }
    with(moduleName: string) {
        // Do stuff to find the module
    }
    private getDeclarationFromName(identifierName: string): AST.ASTDeclaration | undefined {
        if (this.table.has(identifierName)) return this.table.get(identifierName)!;
        if (!this.parent || this.inwardScoping) {
            return undefined;
        } else {
            return this.parent.getDeclarationFromName(identifierName);
        }
    }
    // Scans relevant scopes for a declaration related to the reference (test.other().x)
    getDeclarationFromRef(astRef: AST.ASTReference): AST.ASTDeclaration | undefined {
        switch (astRef.nodeName) {
            case AST.NodeType.NAME:
                let astName = <AST.ASTName>astRef;
                return this.getDeclarationFromName(astName.refName);
            case AST.NodeType.DOT_OP:
                return undefined;
            case AST.NodeType.CALL:
                return undefined;
        }
    }
}

function recurseScope(scope: Scope, fmt: string[]): string[] {
    let lines: string[] = [applyFmt("Scope", fmt, AST.ASTFmt.NODE_FMT)];
    lines.push("   Table:");
    let maxKeyLength = 0;
    let maxTypeLength = 0;
    for (const [key, value] of scope.table.entries()) {
        if (key.length > maxKeyLength) maxKeyLength = key.length;
        if (value.type) {
            let typeName = AST.exprToStr(value.type);
            if (typeName.length > maxTypeLength) maxTypeLength = typeName.length;
        }
    }
    for (const [key, value] of scope.table.entries()) {
        let line =
            "      | " + applyFmt(key, fmt, AST.ASTFmt.IDENTIFIER_FMT) + " ".repeat(maxKeyLength - key.length) + " : ";
        let typeName = "";
        if (value.type) {
            typeName = AST.exprToStr(value.type);
            line += applyFmt(typeName, fmt, AST.ASTFmt.TYPE_FMT);
        }
        if (value.rvalue) {
            line +=
                " ".repeat(maxTypeLength - typeName.length) +
                " = " +
                applyFmt(AST.exprToStr(value.rvalue), fmt, AST.ASTFmt.EXPRESSION_FMT);
        }
        lines.push(line);
    }
    if (scope.children) {
        for (let i = 0; i < scope.children.length; i++) {
            lines.push("   Children:");
            merge(lines, space(3, recurseScope(scope.children[i], fmt)));
        }
    }
    return space(3, lines);
}

export function printScope(scope: Scope) {
    let fmt: string[] = [];
    let lines: string[] = [];
    merge(lines, recurseScope(scope, fmt));

    let str: string[] = [lines.join("\n")];
    for (let i = 0; i < fmt.length; i++) {
        str.push(fmt[i]);
    }
    console.log.apply(lines, str);
}
