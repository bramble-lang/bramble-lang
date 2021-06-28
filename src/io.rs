use std::path::{Path, PathBuf};

use braid_lang::result::NResult;
use clap::ArgMatches;

use crate::{cli::get_imports, manifest::Manifest, BRAID_FILE_EXT};

pub fn get_files(path: &Path, ext: &str) -> Result<Vec<PathBuf>, std::io::Error> {
    let mut files = vec![];
    match path.extension() {
        None => {
            let dir = std::fs::read_dir(path)?;
            for f in dir {
                let f = f?;
                let fty = f.file_type()?;
                if fty.is_file() {
                    match f.path().extension() {
                        Some(ex) if ex.to_ascii_lowercase() == ext => {
                            files.push(f.path());
                        }
                        _ => (),
                    }
                } else if fty.is_dir() {
                    let mut sub_files = get_files(&f.path(), ext)?;
                    files.append(&mut sub_files);
                }
            }
        }
        Some(ex) if ex.to_ascii_lowercase() == "br" => {
            files.push(path.to_path_buf());
        }
        Some(ex) => {
            return Err(std::io::Error::new(
                std::io::ErrorKind::InvalidInput,
                format!(
                    "Is not a Braid language file, expected extension {} but got {}",
                    BRAID_FILE_EXT,
                    ex.to_str().unwrap()
                ),
            ));
        }
    }
    Ok(files)
}

pub fn read_manifests(
    args: &ArgMatches,
) -> NResult<Vec<(crate::ast::Path, Vec<crate::ast::Type>, crate::ast::Type)>> {
    let imports: Vec<_> = get_imports(&args)
        .into_iter()
        .map(|im| {
            std::fs::File::open(im)
                .map_err(|e| format!("Failed to import given project: {}", e))
                .and_then(|mut f| {
                    Manifest::read(&mut f)
                        .map_err(|e| format!("Failed to import given project: {}", e))
                })
                .map(|m| m.get_items())
        })
        .collect();

    let mut oks = vec![];
    let mut errs = vec![];

    for im in imports {
        match im {
            Ok(mut im) => oks.append(&mut im),
            Err(e) => errs.push(e),
        }
    }

    if errs.len() == 0 {
        Ok(oks)
    } else {
        Err(errs)
    }
}
