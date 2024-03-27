use crate::lexer::CodeLoc;

// TODO: extend this later to not only show on which line number, but also which position and which expression went wrong while parsing.
#[derive(Debug)]
pub struct LErr(String, CodeLoc);
impl LErr {
    pub fn lexing_error(message: String, code_loc: CodeLoc) -> Self {
        LErr(message, code_loc)
    }

    pub fn parsing_error(message: String, code_loc: CodeLoc) -> Self {
        LErr(message, code_loc)
    }

    pub fn runtime_error(message: String, code_loc: CodeLoc) -> Self {
        LErr(message, code_loc)
    }

    // TODO type of errors
    pub fn render(self: LErr) {
        let LErr(message, code_loc) = self;
        println!(
            "ERROR: at line [{}] index: [{}]: '{}'",
            code_loc.line, code_loc.index, message
        );
    }
}
