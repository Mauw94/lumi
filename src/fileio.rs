use crate::{LRes, Namespace};

#[derive(Debug)]
pub struct FileIO;

impl Namespace for FileIO {
    fn load_functions(&self, _env: &std::rc::Rc<std::cell::RefCell<crate::Env>>) -> LRes<bool> {
        println!("loading functions..");

        Ok(true)
    }

    fn namespace_name(&self) -> &str {
        "fileio"
    }
}
