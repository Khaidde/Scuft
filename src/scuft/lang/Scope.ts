import * as AST from "./Ast";
import { applyFmt, merge, space, tab, bar } from "./Log";
import { Token, TokenType } from "./Token";

export default class Scope {
    static scopeCounter = 0;
    static newGlobalScope(): Scope {
        let globalScope = new Scope();
        globalScope.operatorOverloads = new Map();
        return globalScope;
    }
    static newScopeFrom(parentScope: Scope): Scope {
        let scope = new Scope();
        scope.parent = parentScope;
        if (!parentScope.debugChildren) parentScope.debugChildren = [];
        parentScope.debugChildren.push(scope);
        return scope;
    }
    static newModuleScopeFrom(parentScope: Scope): Scope {
        let scope = this.newScopeFrom(parentScope);
        scope.unOrdered = true;
        return scope;
    }
    static newTypeDefScopeFrom(parentScope: Scope, typDefDecl: AST.ASTDeclaration): Scope {
        let scope = new Scope();
        if (!parentScope.debugChildren) parentScope.debugChildren = [];
        parentScope.debugChildren.push(scope);
        scope.unOrdered = true;
        scope.addDeclaration(typDefDecl);
        return scope;
    }
    private operatorOverloads!: Map<TokenType, AST.ASTFunction[]>; // Global scope has a list of overloads
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
    table: Map<string, AST.ASTDeclaration[]> = new Map();
    withModules!: AST.ASTDeclaration[]; // Store the scope of all "with" modules
    parent!: Scope;
    unOrdered: boolean = false; // True for module and type definition scopes
    debugChildren!: Scope[]; // Purely for debuging, scope doesn't need to know its children
    private constructor() {}
    searchVariableDeclaration(name: AST.ASTName, excludeDecl?: AST.ASTDeclaration): AST.ASTDeclaration | undefined {
        if (this.table.has(name.refName)) {
            let decl = this.table.get(name.refName)!;
            if (!excludeDecl || !excludeDecl.locToken.equals(decl[0].locToken)) {
                return decl[0];
            }
        }
        if (!this.parent) {
            return undefined; // Couldn't find the declaration
        } else {
            return this.parent.searchVariableDeclaration(name, excludeDecl);
        }
    }
    //Adds declaration to scope without checking for prior existence
    addDeclaration(decl: AST.ASTDeclaration) {
        let refName = (<AST.ASTName>decl.lvalue).refName;
        let existingDecl = this.table.get(refName);
        if (existingDecl) {
            existingDecl.push(decl);
        } else {
            this.table.set(refName, [decl]);
        }
    }
    removeDeclaration(decl: AST.ASTDeclaration) {
        let refName = (<AST.ASTName>decl.lvalue).refName;
        this.table.delete(refName);
    }
    getVariable(name: AST.ASTName): AST.ASTDeclaration | undefined {
        let decl = this.table.get(name.refName);
        if (!decl) return undefined;
        if (decl.length > 1) console.warn("Variable with name " + name.refName + " is a function, not a variable");
        return decl[0];
    }
    printScope() {
        let fmt: string[] = [];
        let lines: string[] = [];
        merge(lines, this.recurseScope(fmt));

        let str: string[] = [lines.join("\n")];
        for (let i = 0; i < fmt.length; i++) {
            str.push(fmt[i]);
        }
        console.log.apply(lines, str);
    }
    recurseScope(fmt: string[]): string[] {
        let lines: string[] = [applyFmt("Scope", fmt)];
        if (this.operatorOverloads) {
            merge(lines, ["   OperatorOverloads:"]);
        }
        lines.push("   Table:");
        let maxKeyLength = 0;
        let maxTypeLength = 0;
        for (const [key, value] of this.table.entries()) {
            if (key.length > maxKeyLength) maxKeyLength = key.length;
            for (let i = 0; i < value.length; i++) {
                if (value[i].resolvedType) {
                    let typeName = AST.exprToStr(value[i].resolvedType);
                    if (typeName.length > maxTypeLength) maxTypeLength = typeName.length;
                }
            }
        }
        for (const [key, value] of this.table.entries()) {
            for (let i = 0; i < value.length; i++) {
                let line = "    |  ";
                if (i === 0) {
                    line += applyFmt(key, fmt, AST.ASTFmt.IDENTIFIER_FMT) + " ".repeat(maxKeyLength - key.length);
                } else {
                    line += " ".repeat(maxKeyLength);
                }
                let typeName = "";
                if (value[i].resolvedType) {
                    typeName = AST.exprToStr(value[i].resolvedType);
                    line += " : " + applyFmt(typeName, fmt, AST.ASTFmt.TYPE_FMT);
                } else {
                    line += "   ";
                }
                if (value[i].rvalue) {
                    let assignment = "  =  ";
                    if (value[i].accessAssignment === TokenType.CONST_ASSIGNMENT_TKN) assignment = "  => ";
                    if (value[i].accessAssignment === TokenType.MUTABLE_ASSIGNMENT_TKN) assignment = " ~=  ";
                    line +=
                        " ".repeat(maxTypeLength - typeName.length) +
                        assignment +
                        applyFmt(AST.exprToStr(value[i].rvalue), fmt, AST.ASTFmt.EXPRESSION_FMT);
                }
                lines.push(line);
            }
        }
        if (this.debugChildren) {
            lines.push("   Children:");
            for (let i = 0; i < this.debugChildren.length; i++) {
                merge(lines, space(3, bar(this.debugChildren[i].recurseScope(fmt))));
            }
        }
        return lines;
    }
}
