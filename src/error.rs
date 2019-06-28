#[derive(Debug, Clone)]
pub struct Span {
    pub start_line: usize,
    pub start_col: usize,
    pub end_line: usize,
    pub end_col: usize,
}

impl Span {
    pub fn new(start_line: usize, start_col: usize, end_line: usize, end_col: usize) -> Self {
        Self {
            start_line,
            start_col,
            end_line,
            end_col,
        }
    }
}

#[derive(Debug, Clone)]
pub struct CompileError {
    pub span: Span,
    pub msg: String,
}

impl CompileError {
    pub fn new(span: Span, msg: &str) -> Self {
        Self {
            span,
            msg: msg.to_string(),
        }
    }
}
