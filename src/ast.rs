use crate::error::Span;

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Int,
    Char,
    Pointer(Box<Type>),
    Array(Box<Type>, usize),
}

impl Type {
    pub fn get_size(&self) -> usize {
        match self {
            Type::Int => 4,
            Type::Char => 1,
            Type::Pointer(_) => 8,
            Type::Array(ty, size) => ty.get_size() * size,
        }
    }
}

#[derive(Debug, Clone)]
pub enum Location {
    Local(usize), // rbpからのオフセット
    Global(String), // ラベル
}

#[derive(Debug, Clone)]
pub struct Variable {
    pub ty: Type,
    pub location: Location,
}

impl Variable {
    pub fn new(ty: Type, location: Location) -> Self {
        Self {
            ty,
            location,
        }
    }
}

#[derive(Debug, Clone)]
pub enum Infix {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    LessThan,
    LessThanOrEqual,
    Equal,
    NotEqual,
    BitAnd,
    BitOr,
    BitXor,
    Shl,
    Shr,
}

#[derive(Debug, Clone)]
pub enum Literal {
    Number(i32),
    String(usize),
}


#[derive(Debug, Clone)]
pub enum ExprKind {
    Literal(Literal),
    Variable(Variable),
    Dereference(Box<Expr>),
    Address(Variable),
    Assign(Box<Expr>, Box<Expr>),
    Infix(Infix, Box<Expr>,  Box<Expr>),
    Call(String, Vec<Expr>),
    BitNot(Box<Expr>),
    SizeOf(Box<Expr>),
    Invalid,
}

#[derive(Debug, Clone)]
pub struct Expr {
    pub kind: ExprKind,
    pub ty: Option<Type>,
    pub span: Span,
}

impl Expr {
    pub fn ty(&self) -> Type {
        self.ty.clone().unwrap()
    }
}

#[derive(Debug, Clone)]
pub enum InitializerKind {
    List(Vec<Initializer>),
    Expr(Expr),
}

#[derive(Debug, Clone)]
pub struct Initializer {
    pub kind: InitializerKind,
    pub span: Span,
}

#[derive(Debug)]
pub enum StmtKind {
    Expr(Expr),
    Return(Expr),
    If(Expr, Box<Stmt>, Option<Box<Stmt>>),
    While(Expr, Box<Stmt>),
    For(Option<Box<Stmt>>, Option<Expr>, Option<Expr>, Box<Stmt>),
    Block(Vec<Stmt>),
    Define(Variable, Option<Initializer>),
}

#[derive(Debug)]
pub struct Stmt {
    pub kind: StmtKind,
    pub span: Span,
}

#[derive(Debug)]
pub enum DeclarationKind {
    Func(String, Type, Vec<Variable>, usize, Stmt), // 関数名, 戻り値の型, 引数, スタックのサイズ, 処理
    GlobalVariable(Variable, Option<Initializer>), // 変数名, 変数, 初期化式
    Prototype(String, Type, Vec<Type>), // 関数名, 戻り値の型, 引数
}

#[derive(Debug)]
pub struct Declaration {
    pub kind: DeclarationKind,
    pub span: Span,
}

#[derive(Debug)]
pub struct Program {
    pub declarations: Vec<Declaration>,
    pub global_variables: Vec<Variable>,
    pub string_list: Vec<String>,
}
