use std::path::{Path, PathBuf};

use crate::{
    compiler::{
        ast::Module,
        lexer::{tokens::Token, LexerError},
        parser::{self, ParserContext, ParserError},
        CompilerDisplay, CompilerDisplayError, CompilerError, SourceCharIter, SourceMap,
        SourceMapError, Span,
    },
    StringId, StringTable,
};
use crate::{diagnostics::config::TracingConfig, io::get_files};

#[derive(Debug)]
pub enum ProjectError {
    NoAstGenerated,
    InvalidPath,
    ParserError(ParserError),
}

impl From<CompilerError<ParserError>> for CompilerError<ProjectError> {
    fn from(parser_ce: CompilerError<ParserError>) -> Self {
        let (line, ie) = parser_ce.take();
        CompilerError::new(line, ProjectError::ParserError(ie))
    }
}

impl CompilerDisplay for ProjectError {
    fn fmt(&self, st: &StringTable) -> Result<String, CompilerDisplayError> {
        match self {
            ProjectError::NoAstGenerated => Ok("No AST Generated by Parser".into()),
            ProjectError::InvalidPath => Ok("Invalid compilation unit: path was empty".into()),
            ProjectError::ParserError(pe) => pe.fmt(st),
        }
    }
}

/// Given the path to a source, return the name that should be used
/// for the project.
/// If the path is a file, then return the file name (without extension)
/// If the path is a directory, then return the name of the directory
pub fn get_project_name(src: &Path) -> Result<&str, String> {
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
    module: &'a mut Module<ParserContext>,
    path: &[StringId],
) -> Option<&'a mut Module<ParserContext>> {
    match path.split_first() {
        Some((head, rest)) => {
            if module.get_module(*head).is_none() {
                let sub = Module::new(*head, ParserContext::new(0, Span::zero()));
                module.add_module(sub);
            }

            let sub = module
                .get_module_mut(*head)
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

/// Given the location for source code files, this will build a [`SourceMap`]
/// which can then be used to access all the source code for the target project.
pub fn build_source_map(
    src_path: &std::path::Path,
    ext: &str,
) -> Result<SourceMap, SourceMapError> {
    let mut sm = SourceMap::new();

    let files = get_files(&src_path, ext)?;
    for file in files {
        sm.add_file(file)?;
    }

    Ok(sm)
}

/// Parses every tokenized compilation unit in the given vector.
/// Each compilation unit is parsed into a module named after the
/// path given in the CompilationUnit and all are added as child
/// modules of a single "root" module.
pub fn parse_project(
    string_table: &mut StringTable,
    root_module: StringId,
    token_sets: Project<Vec<Token>>,
    trace_parser: TracingConfig,
) -> Result<Module<ParserContext>, Vec<CompilerError<ProjectError>>> {
    parser::parser::set_tracing(trace_parser);
    let mut root = Module::new(root_module, ParserContext::new(0, Span::zero())); // TODO: pass in the source map and get the span that covers everything in the source map and make it the root span
    let mut errors = vec![];
    for src_tokens in token_sets {
        match parse_src_tokens(string_table, src_tokens) {
            Ok(ast) => append_module(string_table, &mut root, ast),
            Err(e) => errors.push(e),
        }
    }
    if errors.len() == 0 {
        Ok(root)
    } else {
        Err(errors)
    }
}

/// For each compilation unit in the [`SourceMap`], tokenize, and add to a vector
/// of tokenized compilation units.
pub fn tokenize_source_map(
    string_table: &mut StringTable,
    sourcemap: SourceMap,
    trace_lexer: TracingConfig,
    src_path: &std::path::Path,
) -> Result<Vec<CompilationUnit<Vec<Token>>>, Vec<CompilerError<LexerError>>> {
    let mut project_token_sets = vec![];

    // Iterate through each entry in the source map
    for idx in 0..sourcemap.len() {
        let entry = sourcemap.get(idx).unwrap();

        // Derive the logical path within the project
        let module_path = file_path_to_module_path(entry.path(), src_path);

        // Create a compilation Unit from the SourceCharIter
        let src = CompilationUnit {
            path: module_path,
            data: entry.chars(),
        };

        // Get the Token Set and add to the Vector of token sets
        let tokens = tokenize_source(string_table, src, trace_lexer)?;
        project_token_sets.push(tokens);
    }

    Ok(project_token_sets)
}

/// Tokenizes a stream of unicode characters.
fn tokenize_source(
    string_table: &mut StringTable,
    src: CompilationUnit<SourceCharIter>,
    trace_lexer: TracingConfig,
) -> Result<CompilationUnit<Vec<Token>>, Vec<CompilerError<LexerError>>> {
    let mut lexer = crate::compiler::Lexer::new(string_table, src.data).unwrap();
    lexer.set_tracing(trace_lexer);
    let tokens = lexer.tokenize();
    let (tokens, errors): (
        Vec<std::result::Result<Token, _>>,
        Vec<std::result::Result<Token, _>>,
    ) = tokens.into_iter().partition(|t| t.is_ok());

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
        let errors: Vec<_> = errors
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
    string_table: &mut StringTable,
    src_tokens: CompilationUnit<Vec<Token>>,
) -> Result<CompilationUnit<Module<ParserContext>>, CompilerError<ProjectError>> {
    if let Some((name, parent_path)) = src_tokens.path.split_last() {
        let name = string_table.insert(name.into());
        match parser::parser::parse(name, &src_tokens.data) {
            Ok(Some(ast)) => Ok(CompilationUnit {
                path: parent_path.to_owned(),
                data: ast,
            }),
            Ok(None) => Err(CompilerError::new(0, ProjectError::NoAstGenerated)),
            Err(msg) => Err(msg.into()),
        }
    } else {
        Err(CompilerError::new(0, ProjectError::InvalidPath))
    }
}

fn append_module(
    string_table: &mut StringTable,
    root: &mut Module<ParserContext>,
    src_ast: CompilationUnit<Module<ParserContext>>,
) {
    let parent = if src_ast.path.len() == 0 {
        root
    } else {
        let path: Vec<_> = src_ast
            .path
            .iter()
            .map(|p| string_table.insert(p.into()))
            .collect();
        create_module_path(root, &path).unwrap()
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
