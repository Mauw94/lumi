use std::{
    cell::RefCell,
    fs::{self, File},
    io::{BufRead, BufReader},
    path::Path,
    rc::Rc,
};

use crate::{
    check_args, get_list_from_arg_obj, get_str_from_args_vec_obj, try_borrow_mut, vectors, Builtin, CodeLoc, Env, LErr, LRes, LResult, Namespace, NamespaceType, Obj, Seq
};

#[derive(Debug)]
pub struct FileIO;

impl Namespace for FileIO {
    fn load_functions(&self, env: &Rc<RefCell<Env>>) -> LRes<()> {
        let mut e = try_borrow_mut(env)?;

        e.insert_builtin(
            ReadFile,
            NamespaceType::External(FileIO.namespace_name().to_string()),
        );
        e.insert_builtin(
            ByteToString,
            NamespaceType::External(FileIO.namespace_name().to_string()),
        );
        e.insert_builtin(
            ReadFileLines,
            NamespaceType::External(FileIO.namespace_name().to_string()),
        );

        Ok(())
    }

    fn get_function_names(&self) -> Vec<String> {
        vec![
            ReadFile.builtin_name().to_string(),
            ByteToString.builtin_name().to_string(),
            ReadFileLines.builtin_name().to_string(),
        ]
    }

    fn namespace_name(&self) -> &str {
        "fileio"
    }

    fn unload_functions(&self, env: &Rc<RefCell<Env>>) -> LRes<()> {
        let mut e = try_borrow_mut(env)?;

        e.remove_function(ReadFile.builtin_name())?;
        e.remove_function(ByteToString.builtin_name())?;
        e.remove_function(ReadFileLines.builtin_name())?;

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
                    Ok(contents) => match vectors::parse_u8vec_to_lumi_vec(contents) {
                        Ok(res) => return Ok(Obj::LResult(LResult::Res(Box::new(res)))),
                        Err(e) => return Err(e),
                    },
                    Err(e) => {
                        eprintln!("{:?}", e.to_string());
                        return Ok(Obj::Null);
                        // return Ok(Obj::LResult(LResult::Error(e.to_string())));
                    }
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
struct ReadFileLines;

impl Builtin for ReadFileLines {
    fn run(
        &self,
        _env: &Rc<RefCell<Env>>,
        args: Vec<Obj>,
        start: CodeLoc,
        end: CodeLoc,
    ) -> LRes<Obj> {
        check_args(1, 1, &args, start, end)?;

        let path = get_str_from_args_vec_obj(0, &args)?;
        let file_loc = Path::new(&path);

        let file = match File::open(file_loc) {
            Ok(f) => f,
            Err(e) => return Err(LErr::internal_error(e.to_string())),
        };

        let reader = BufReader::new(file);
        let lines: Vec<String> = match reader.lines().collect::<Result<_, _>>() {
            Ok(lines) => lines,
            Err(e) => return Err(LErr::internal_error(e.to_string())),
        };
        let lines_res: Vec<Obj> = lines
            .iter()
            .map(|l| Obj::Seq(Seq::String(Rc::new(l.to_string()))))
            .collect();

        Ok(Obj::Seq(Seq::List(Rc::new(RefCell::new(lines_res)))))
    }

    fn builtin_name(&self) -> &str {
        "read_file_lines"
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
        let list_val = get_list_from_arg_obj(0, &args)?;
        let list = list_val.borrow();
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
