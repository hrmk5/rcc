use crate::error::Span;

pub fn align(stack_size: usize, ty: &Type) -> usize {
    let align = ty.align();
    let mut stack_size = stack_size;
    let padding = align - stack_size % align;
    if padding != align {
        stack_size += padding;
    }
    stack_size
}

#[derive(Debug, Clone)]
pub enum Type {
    Int,
    Char,
    Short,
    Long,
    Float,
    Double,
    Void,
    Pointer(Box<Type>),
    Array(Box<Type>, usize),
    Structure(Option<String>, Vec<(String, Variable)>, usize),
    Const(Box<Type>),
}

impl Type {
    pub fn new_structure(name: Option<String>, member_types: Vec<(String, Type)>) -> Self {
        let mut members = Vec::new();
        let mut size = 0;

        for (name, ty) in member_types {
            let align = ty.align();
            let padding = align - size % align;
            if padding != align {
                size += padding;
            }

            members.push((name, Variable::new(ty.clone(), Location::Local(size))));
            size += ty.get_size();
        }

        Type::Structure(name, members, size)
    }

    pub fn get_size(&self) -> usize {
        match self {
            Type::Int => 4,
            Type::Char => 1,
            Type::Short => 2,
            Type::Long => 8,
            Type::Float => 4,
            Type::Double => 8,
            Type::Void => 8,
            Type::Pointer(_) => 8,
            Type::Array(ty, size) => ty.get_size() * size,
            Type::Structure(_, _, size) => *size,
            Type::Const(ty) => ty.get_size(),
        }
    }

    pub fn align(&self) -> usize {
        match self {
            Type::Structure(_, members, _) => members.iter()
                .max_by_key(|(_, var)| var.ty.align())
                .map_or(0, |(_, var)| var.ty.align()),
            Type::Array(ty, _) | Type::Const(ty) => ty.align(),
            ty => ty.get_size(),
        }
    }

    pub fn find_member(&self, name: &str) -> &Variable {
        match self {
            Type::Structure(_, members, _) => match members.iter().find(|(name_, _)| name == name_) {
                Some((_, var)) => var,
                _ => panic!("メンバが見つかりません"),
            },
            _ => panic!("構造体ではありません"),
        }
    }

    pub fn is_number(&self) -> bool {
        match self {
            Type::Array(_, _) | Type::Structure(_, _, _) | Type::Void => false,
            Type::Const(ty) => ty.is_number(),
            _ => true,
        }
    }

    pub fn is_integer(&self) -> bool {
        self.is_number() && !self.is_floating_number()
    }

    pub fn is_floating_number(&self) -> bool {
        match self {
            Type::Float | Type::Double => true,
            _ => false,
        }
    }

    pub fn can_assign_to(&self, ty: &Type) -> bool {
        let lty = ty;
        let rty = self;

        match lty {
            Type::Void | Type::Array(_, _) => false,
            Type::Pointer(_) => match rty {
                Type::Array(_, _) => true,
                rty => rty.is_number(),
            },
            Type::Const(lty) => rty.can_assign_to(lty),
            lty if lty.is_number() => rty.is_number(),
            Type::Structure(lname, _, _) => match rty {
                Type::Structure(rname, _, _) => lname == rname && lname.is_some() && rname.is_some(),
                _ => false,
            },
            _ => panic!(),
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

    pub fn offset(&self) -> usize {
        match self.location {
            Location::Local(offset) => offset,
            _ => panic!("ローカル変数ではありません"),
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
    Number(i64),
    Float(usize),
    Double(usize),
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
    MemberAccess(Box<Expr>, String),
    Increment(Box<Expr>, bool),
    Decrement(Box<Expr>, bool),
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
    Switch(Expr, Vec<Expr>, Box<Stmt>, bool),
    Break,
    Case(Expr),
    Continue,
    Default,
}

#[derive(Debug)]
pub struct Stmt {
    pub kind: StmtKind,
    pub span: Span,
}

#[derive(Debug)]
pub enum DeclarationKind {
    Func(String, Type, Vec<Variable>, usize, Stmt, bool), // 関数名, 戻り値の型, 引数, スタックのサイズ, 処理, staticかどうか
    GlobalVariable(Variable, Option<Initializer>, bool), // 変数名, 変数, 初期化式, staticかどうか
    Prototype(String, Type, Vec<Type>), // 関数名, 戻り値の型, 引数
    Extern(Box<Declaration>),
}

#[derive(Debug)]
pub struct Declaration {
    pub kind: DeclarationKind,
    pub span: Span,
}

impl Declaration {
    // 識別子を取得する
    #[allow(dead_code)]
    pub fn get_identifier(&self) -> String {
        match &self.kind {
            DeclarationKind::Func(name, _, _, _, _, _) => name.clone(),
            DeclarationKind::GlobalVariable(var, _, _) => match &var.location {
                Location::Global(name) => name.clone(),
                _ => panic!(),
            },
            DeclarationKind::Prototype(name, _, _) => name.clone(),
            DeclarationKind::Extern(decl) => decl.get_identifier(),
        }
    }
}

#[derive(Debug)]
pub struct Program {
    pub declarations: Vec<Declaration>,
    pub global_variables: Vec<Variable>,
    pub string_list: Vec<String>,
    pub float_list: Vec<f32>,
    pub double_list: Vec<f64>,
}
