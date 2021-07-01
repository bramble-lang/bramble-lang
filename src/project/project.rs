use std::path::{Path, PathBuf};

use crate::compiler::{ast::Module, lexer::tokens::Token, parser};
use crate::{diagnostics::config::TracingConfig, io::get_files};

use braid_lang::result::{NResult, Result};

/// Given the path to a source, return the name that should be used
/// for the project.
/// If the path is a file, then return the file name (without extension)
/// If the path is a directory, then return the name of the directory
pub fn get_project_name(src: &Path) -> Result<&str> {
    if src.is_file() || src.is_dir() {
        src.file_stem()
            .map(|name| name.to_str())
            .flatten()
            .ok_or("Could not extract name from given path".into())
    } else {
        Err("Given path is neither a directory or a file".into())
    }
}

fn create_module_path<'a>(
    module: &'a mut Module<u32>,
    path: &[String],
) -> Option<&'a mut Module<u32>> {
    match path.split_first() {
        Some((head, rest)) => {
            if module.get_module(head).is_none() {
                let sub = Module::new(head, 0);
                module.add_module(sub);
            }

            let sub = module
                .get_module_mut(head)
                .expect("A module with this name was just created and ought to be found");

            if rest.len() == 0 {
                Some(sub)
            } else {
                create_module_path(sub, rest)
            }
        }
        None => None,
    }
}

type Project<T> = Vec<CompilationUnit<T>>;

pub struct CompilationUnit<T> {
    path: Vec<String>,
    data: T,
}

/// Given the location of source file(s) this function will read the file
/// or files and return a list of the text of all source files. For each
/// file there will also be the associated path to the file (relative to
/// `src_path`).
///
/// If `src_path` is a directory, this will recursively read every file in
/// that directory and its subdirectories.  If it is a file, it will read
/// only that file.
pub fn read_src_files(src_path: &std::path::Path, ext: &str) -> Vec<CompilationUnit<String>> {
    let files = get_files(&src_path, ext).expect(&format!("Could not open: {:?}", src_path));
    let mut texts: Vec<CompilationUnit<String>> = vec![];
    for file in files {
        let p = file_path_to_module_path(&file, &src_path);

        let text = std::fs::read_to_string(&file)
            .expect(&format!("Failed to read input file: {:?}", file));
        texts.push(CompilationUnit {
            path: p,
            data: text,
        });
    }

    texts
}

/// Parses every tokenized compilation unit in the given vector.
/// Each compilation unit is parsed into a module named after the
/// path given in the CompilationUnit and all are added as child
/// modules of a single "root" module.
pub fn parse_project(
    root_module: &str,
    token_sets: Project<Vec<Token>>,
    trace_parser: TracingConfig,
) -> NResult<Module<u32>> {
    parser::parser::set_tracing(trace_parser);
    let mut root = Module::new(root_module, 0);
    let mut errors = vec![];
    for src_tokens in token_sets {
        match parse_src_tokens(src_tokens) {
            Ok(ast) => append_module(&mut root, ast),
            Err(e) => errors.push(e),
        }
    }
    if errors.len() == 0 {
        Ok(root)
    } else {
        Err(errors)
    }
}

pub fn tokenize_project(
    src_input: Project<String>,
    trace_lexer: TracingConfig,
) -> NResult<Vec<CompilationUnit<Vec<Token>>>> {
    let mut token_sets = vec![];
    let mut errors = vec![];
    for src in src_input {
        match tokenize_src_file(src, trace_lexer) {
            Ok(t) => token_sets.push(t),
            Err(mut e) => errors.append(&mut e),
        }
    }

    if errors.len() == 0 {
        Ok(token_sets)
    } else {
        Err(errors)
    }
}

fn tokenize_src_file(
    src: CompilationUnit<String>,
    trace_lexer: TracingConfig,
) -> NResult<CompilationUnit<Vec<Token>>> {
    let mut lexer = crate::compiler::Lexer::new(&src.data);
    lexer.set_tracing(trace_lexer);
    let tokens = lexer.tokenize();
    let (tokens, errors): (Vec<Result<Token>>, Vec<Result<Token>>) =
        tokens.into_iter().partition(|t| t.is_ok());

    if errors.len() == 0 {
        let tokens: Vec<Token> = tokens
            .into_iter()
            .filter_map(|t| match t {
                Ok(token) => Some(token),
                Err(_) => None,
            })
            .collect();
        Ok(CompilationUnit {
            path: src.path,
            data: tokens,
        })
    } else {
        let errors: Vec<String> = errors
            .into_iter()
            .filter_map(|t| match t {
                Ok(_) => None,
                Err(msg) => Some(msg),
            })
            .collect();
        Err(errors)
    }
}

/// Takes CompilationUnit which has been tokenized and parses the tokens into
/// an AST.
///
/// The last element of the compilation unit's path (the name of the module derived
/// from the source file name).  Will be removed from the path vector, because it becomes
/// part of the data field (when a module is created with the same name that becomes the
/// parent of all items within the source file).
fn parse_src_tokens(
    src_tokens: CompilationUnit<Vec<Token>>,
) -> Result<CompilationUnit<Module<u32>>> {
    if let Some((name, parent_path)) = src_tokens.path.split_last() {
        match parser::parser::parse(name, &src_tokens.data) {
            Ok(Some(ast)) => Ok(CompilationUnit {
                path: parent_path.to_owned(),
                data: ast,
            }),
            Ok(None) => Err("Critical: no AST was generated by the parser".into()),
            Err(msg) => Err(format!("Error: {}", msg)),
        }
    } else {
        Err("Invalid compilation unit path: was empty".into())
    }
}

fn append_module(root: &mut Module<u32>, src_ast: CompilationUnit<Module<u32>>) {
    let parent = if src_ast.path.len() == 0 {
        root
    } else {
        create_module_path(root, &src_ast.path).unwrap()
    };
    parent.add_module(src_ast.data);
}

fn file_path_to_module_path(file: &PathBuf, src_path: &Path) -> Vec<String> {
    let fpath = file.as_path();
    let base = if src_path.is_dir() {
        src_path
    } else {
        src_path
            .parent()
            .expect("Given a file which is also the root of the directory structure.")
    };

    let rel_path = fpath.strip_prefix(&base).unwrap();

    let mut p: Vec<String> = rel_path
        .iter()
        .map(|e| e.to_str().expect("File name was not valid unicode").into())
        .collect();

    truncate_extension(&mut p, ".br");
    p
}

fn truncate_extension(path: &mut Vec<String>, ext: &str) {
    match path.last_mut() {
        Some(l) if l.ends_with(ext) => l.truncate(l.len() - ext.len()),
        _ => (),
    }
}
