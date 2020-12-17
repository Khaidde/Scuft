import Token from "./Token";

export class ASTProgram {
    statements: ASTStatement[] = [];
}

export class ASTStatement {}

export class ASTExpression {
    lvalue!: ASTExpression;
    rvalue!: ASTExpression;

    value!: Token; // numeric literal, Identifier (maybe later string literals)
}

export class ASTBlock {}

export class ASTDeclaration extends ASTStatement {
    lvalue!: Token; //Identifier

    // optional (but at least 1 needed)
    type!: ASTType; //Identifier or Type
    rvalue!: ASTExpression | ASTBlock; // value or a block
}

export class ASTType {
    inputType!: ASTDeclaration[]; // "input" or actual type
    outType!: ASTType; // "output"

    baseType!: Token; // num, string, boolean, vector
}

export class ASTTypeDeclaration extends ASTStatement {
    typeDeclarations!: ASTDeclaration[];
}
