// TODO: extend this later to not only show on which line number, but also which position and which expression went wrong while parsing.
#[derive(Debug)]
pub struct LErr(String, i8);
impl LErr {
    pub fn lexing_error(message: String, line: i8) -> Self {
        LErr(message, line)
    }

    pub fn parsing_error(message: String, line: i8) -> Self {
        LErr(message, line)
    }

    pub fn render(self: LErr) {
        let LErr(message, line) = self;
        println!("Error occured on line [{}] '{}'", line, message);
    }
}
