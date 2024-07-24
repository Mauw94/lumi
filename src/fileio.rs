use std::{cell::RefCell, fs, path::Path, rc::Rc};

use crate::{
    check_args, get_list_from_arg_obj, get_str_from_args_vec_obj, vectors, Builtin, CodeLoc, Env,
    LErr, LRes, Namespace, NamespaceType, Obj, Seq,
};

#[derive(Debug)]
pub struct FileIO;

impl Namespace for FileIO {
    fn get_function_names(&self) -> Vec<String> {
        vec!["read_file".to_string(), "byte_to_string".to_string()]
    }

    fn load_functions(&self, env: &Rc<RefCell<Env>>) -> LRes<()> {
        let mut e = env.borrow_mut();
        e.insert_builtin(
            ReadFile,
            NamespaceType::External(FileIO.namespace_name().to_string()),
        );
        e.insert_builtin(
            ByteToString,
            NamespaceType::External(FileIO.namespace_name().to_string()),
        );

        Ok(())
    }

    fn namespace_name(&self) -> &str {
        "fileio"
    }

    fn unload_functions(&self, env: &Rc<RefCell<Env>>) -> LRes<()> {
        let mut e = env.borrow_mut();
        e.remove_builtin(ReadFile.builtin_name())?;
        e.remove_builtin(ByteToString.builtin_name())?;

        Ok(())
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
                    Ok(contents) => return vectors::parse_u8vec_to_lumi_vec(contents),
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

#[derive(Debug)]
struct ByteToString;

impl Builtin for ByteToString {
    fn run(
        &self,
        _env: &Rc<RefCell<Env>>,
        args: Vec<Obj>,
        start: CodeLoc,
        end: CodeLoc,
    ) -> LRes<Obj> {
        check_args(1, 1, &args, start, end)?;
        let list = get_list_from_arg_obj(0, &args)?;
        let vec: Vec<u8> = vectors::parse_lumi_list_to_rust_vec::<u8>(&list)?;

        match String::from_utf8(vec) {
            Ok(text) => return Ok(Obj::Seq(Seq::String(Rc::new(text)))),
            Err(e) => return Err(LErr::internal_error(e.to_string())),
        };
    }

    fn builtin_name(&self) -> &str {
        "byte_to_str"
    }
}
