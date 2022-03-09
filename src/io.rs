use std::path::{Path, PathBuf};

use crate::{result::NResult, Manifest};
use clap::ArgMatches;

use super::cli::get_imports;

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
                    "Is not a Bramble language file, expected extension {} but got {}",
                    ext,
                    ex.to_str().unwrap()
                ),
            ));
        }
    }
    Ok(files)
}

pub fn read_manifests(args: &ArgMatches) -> NResult<Vec<Manifest>> {
    let imports: Vec<_> = get_imports(args)
        .into_iter()
        .map(|im| {
            std::fs::File::open(im)
                .map_err(|e| format!("{}", e))
                .and_then(|mut f| Manifest::read(&mut f).map_err(|e| format!("{}", e)))
                .map_err(|e| format!("Failed to import {}: {}", im, e))
        })
        .collect();

    let mut oks = vec![];
    let mut errs = vec![];

    for im in imports {
        match im {
            Ok(im) => oks.push(im),
            Err(e) => errs.push(e),
        }
    }

    if errs.is_empty() {
        Ok(oks)
    } else {
        Err(errs)
    }
}
