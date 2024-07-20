use std::{cell::RefCell, rc::Rc};

use crate::{Builtin, CodeLoc, Env, LRes, Namespace, Obj};

#[derive(Debug)]
pub struct FileIO;

impl Namespace for FileIO {
    fn load_functions(&self, env: &Rc<RefCell<Env>>) -> LRes<()> {
        println!("loading functions..");

        let mut e = env.borrow_mut();
        e.insert_builtin(TestFile);

        Ok(())
    }

    fn namespace_name(&self) -> &str {
        "fileio"
    }

    fn unload_functions(&self, env: &Rc<RefCell<Env>>) -> LRes<()> {
        println!("unloading functions...");

        let mut e = env.borrow_mut();
        e.remove_builtin(TestFile.builtin_name())?;

        Ok(())
    }
}

#[derive(Debug)]
struct TestFile;

impl Builtin for TestFile {
    fn run(
        &self,
        _env: &std::rc::Rc<std::cell::RefCell<Env>>,
        _args: Vec<Obj>,
        _start: CodeLoc,
        _end: CodeLoc,
    ) -> LRes<Obj> {
        println!("Running TestFile from FileIO");
        Ok(Obj::Null)
    }

    fn builtin_name(&self) -> &str {
        "test_file"
    }
}
