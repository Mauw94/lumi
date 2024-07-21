use std::{cell::RefCell, fs, path::Path, rc::Rc};

use crate::{
    check_args, get_str_from_args_vec_obj, vectors, Builtin, CodeLoc, Env, LErr, LRes, Namespace,
    Obj,
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
                let path = get_str_from_args_vec_obj(0, &args)?;
                let file_loc = Path::new(&path);
                match fs::read(file_loc) {
                    Ok(contents) => {
                        let res = vectors::parse_u8_to_lumi_vec(contents);

                        match &res {
                            Ok(r) => {
                                let lst = r.get_list_val()?;
                                // TODO: from here we can build a function that reads the lumi_u8_vec and returns the original text of the read file.
                                let vec: Vec<u8> =
                                    vectors::get_list_values_to_rust_vec::<u8>(&lst)?;
                                println!("ORIGINAL VEC{:?}", vec);
                            }
                            Err(_) => todo!(),
                        }

                        return res;
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
