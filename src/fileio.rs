use std::{cell::RefCell, fs, path::Path, rc::Rc};

use crate::{
    check_args, get_str_from_args_vec_obj, Builtin, CodeLoc, Env, LErr, LNum, LRes, Namespace, Obj,
    Seq,
};

#[derive(Debug)]
pub struct FileIO;

impl Namespace for FileIO {
    fn load_functions(&self, env: &Rc<RefCell<Env>>) -> LRes<()> {
        println!("loading FILEIO functions..");

        let mut e = env.borrow_mut();
        e.insert_builtin(TestFile);
        e.insert_builtin(ReadFile);

        Ok(())
    }

    fn namespace_name(&self) -> &str {
        "fileio"
    }

    fn unload_functions(&self, env: &Rc<RefCell<Env>>) -> LRes<()> {
        println!("unloading FILEIO functions...");

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

#[derive(Debug)]
struct ReadFile;

impl Builtin for ReadFile {
    fn run(
        &self,
        _env: &Rc<RefCell<Env>>,
        args: Vec<Obj>,
        start: CodeLoc,
        end: CodeLoc,
    ) -> LRes<Obj> {
        check_args(1, 2, &args, start, end)?;

        match args.len() {
            1 => {
                println!("1 arg");
                let path = get_str_from_args_vec_obj(0, &args)?;
                println!("{:?}", path);
                let file_loc = Path::new(&path);
                match fs::read(file_loc) {
                    Ok(contents) => {
                        println!("{:?}", contents);
                        // TODO: move to Vectors
                        let lumi_vec: Vec<Obj> =
                            contents.iter().map(|b| Obj::Num(LNum::Byte(*b))).collect();

                        return Ok(Obj::Seq(Seq::List(Rc::new(lumi_vec))));
                    }
                    Err(e) => return Err(LErr::internal_error(e.to_string())),
                }
            }
            2 => println!("2 args"),
            _ => println!("no args? impossible to get here tho."),
        }

        Ok(Obj::Null)
    }

    fn builtin_name(&self) -> &str {
        "read_file"
    }
}
