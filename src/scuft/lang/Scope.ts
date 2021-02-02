import * as AST from "./Ast";
import { applyFmt, merge, space, bar } from "./Log";
import { TokenType, tokenToStr } from "./Token";

export enum ScopeType {
    GLOBAL_SCOPE,
    MODULE_SCOPE,
    TYPE_SCOPE,

    FUNC_SCOPE,

    BRANCH_SCOPE,
    LOOP_SCOPE,
}
export class Scope {
    static newGlobalScope(): Scope {
        let globalScope = new Scope(undefined);
        globalScope.operatorOverloads = new Map();
        globalScope.scopeType = ScopeType.GLOBAL_SCOPE;
        return globalScope;
    }
    operatorOverloads!: Map<TokenType, AST.ASTFunction[]>; // Global scope has a list of overloads
    addOperatorOverload(operator: TokenType, overload: AST.ASTFunction) {
        let existingOverloads = this.operatorOverloads.get(operator);
        if (!existingOverloads) {
            existingOverloads = [];
            this.operatorOverloads.set(operator, existingOverloads);
        }
        existingOverloads.push(overload);
    }
    getOperatorOverload(operator: TokenType): AST.ASTFunction[] | undefined {
        return this.operatorOverloads.get(operator);
    }
    static scopeCounter = 0;
    scopeID: number;
    scopeType!: ScopeType;
    table: Map<string, AST.ASTDeclaration> = new Map();
    withModules!: AST.ASTDeclaration[]; // Store the scope of all "with" modules
    parent!: Scope;
    debugChildren!: Scope[]; // Purely for debuging, scope doesn't need to know its children
    constructor(parentScope: Scope | undefined) {
        this.scopeID = Scope.scopeCounter++;
        if (parentScope) {
            this.parent = parentScope;
            if (!parentScope.debugChildren) parentScope.debugChildren = [];
            parentScope.debugChildren.push(this);
        }
    }
    getEnclosingScope(scopeType: ScopeType): Scope | undefined {
        if (this.scopeType === scopeType) return this;
        if (!this.parent) return undefined;
        return this.parent.getEnclosingScope(scopeType);
    }
    equals(otherScope: Scope) {
        return otherScope.scopeID === this.scopeID;
    }
    isUnordered() {
        return (
            this.scopeType === ScopeType.GLOBAL_SCOPE ||
            this.scopeType === ScopeType.MODULE_SCOPE ||
            this.scopeType === ScopeType.TYPE_SCOPE
        );
    }
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
function recurseScope(scope: Scope, fmt: string[]): string[] {
    let lines: string[] = [applyFmt("Scope", fmt)];
    if (scope.operatorOverloads) {
        merge(lines, ["   OperatorOverloads:"]);
        let maxKeyLength = 0;
        for (const key of scope.operatorOverloads.keys()) {
            if (tokenToStr(key).length > maxKeyLength) maxKeyLength = tokenToStr(key).length;
        }
        for (const [key, value] of scope.operatorOverloads.entries()) {
            for (let i = 0; i < value.length; i++) {
                let line = "    |  ";
                if (i === 0) {
                    line +=
                        applyFmt(tokenToStr(key), fmt, AST.ASTFmt.IDENTIFIER_FMT) +
                        " ".repeat(maxKeyLength - tokenToStr(key).length);
                } else {
                    line += " ".repeat(maxKeyLength);
                }
                line += "  => " + applyFmt(AST.exprToStr(value[i]), fmt, AST.ASTFmt.EXPRESSION_FMT);
                lines.push(line);
            }
        }
    }
    lines.push("   Table:");
    let maxKeyLength = 0;
    let maxTypeLength = 0;
    for (const [key, value] of scope.table.entries()) {
        if (key.length > maxKeyLength) maxKeyLength = key.length;
        if (value.type && value.type.resolved) {
            let typeName = AST.exprToStr(value.type.resolved);
            if (typeName.length > maxTypeLength) maxTypeLength = typeName.length;
        }
    }
    for (const [key, value] of scope.table.entries()) {
        let line = "    |  ";
        line += applyFmt(key, fmt, AST.ASTFmt.IDENTIFIER_FMT) + " ".repeat(maxKeyLength - key.length);
        let typeName = "";
        if (value.type && value.type.resolved) {
            typeName = AST.exprToStr(value.type.resolved);
            line += " : " + applyFmt(typeName, fmt, AST.ASTFmt.TYPE_FMT);
        } else {
            line += "   ";
        }
        if (value.rvalue) {
            let assignment = "  =  ";
            if (value.accessAssignment.type === TokenType.CONST_ASSIGNMENT_TKN) assignment = "  => ";
            if (value.accessAssignment.type === TokenType.MUTABLE_ASSIGNMENT_TKN) assignment = " ~=  ";
            line +=
                " ".repeat(maxTypeLength - typeName.length) +
                assignment +
                applyFmt(AST.exprToStr(value.rvalue), fmt, AST.ASTFmt.EXPRESSION_FMT);
        }
        lines.push(line);
    }
    if (scope.debugChildren) {
        lines.push("   Children:");
        for (let i = 0; i < scope.debugChildren.length; i++) {
            merge(lines, space(3, bar(recurseScope(scope.debugChildren[i], fmt))));
        }
    }
    return lines;
}
