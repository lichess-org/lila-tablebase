use std::{
    collections::HashMap,
    ffi::OsString,
    fs, io,
    path::{Path, PathBuf},
};

use shakmaty_syzygy::filesystem::{Filesystem, RandomAccessFile, ReadHint};

pub struct HotPrefixFilesystem {
    underlying: Box<dyn Filesystem>,
    prefix_candidates: HashMap<OsString, PathBuf>,
}

impl HotPrefixFilesystem {
    pub fn new(underlying: Box<dyn Filesystem>) -> HotPrefixFilesystem {
        HotPrefixFilesystem {
            underlying,
            prefix_candidates: HashMap::new(),
        }
    }

    pub fn add_prefix_directory(&mut self, path: &Path) -> io::Result<usize> {
        let mut n = 0;
        for entry in fs::read_dir(path)? {
            let entry = entry?;
            let path = entry.path();
            if path.extension().is_none_or(|ext| ext != "prefix") {
                continue;
            }
            if !entry.metadata()?.is_file() {
                continue;
            }
            if let Some(name) = path.file_stem() {
                self.prefix_candidates.insert(name.to_owned(), path);
                n += 1;
            }
        }
        Ok(n)
    }
}

impl Filesystem for HotPrefixFilesystem {
    fn regular_file_size(&self, path: &Path) -> io::Result<u64> {
        self.underlying.regular_file_size(path)
    }

    fn read_dir(&self, path: &Path) -> io::Result<Vec<PathBuf>> {
        self.underlying.read_dir(path)
    }

    fn open(&self, path: &Path) -> io::Result<Box<dyn RandomAccessFile>> {
        Ok(Box::new(
            if let Some(prefix_file_path) = path
                .file_name()
                .and_then(|name| self.prefix_candidates.get(name))
            {
                HotPrefixRandomAccessFile {
                    prefix_len: self.regular_file_size(prefix_file_path)?,
                    prefix_file: Some(self.underlying.open(prefix_file_path)?),
                    file: self.underlying.open(path)?,
                }
            } else {
                HotPrefixRandomAccessFile {
                    prefix_len: 0,
                    prefix_file: None,
                    file: self.underlying.open(path)?,
                }
            },
        ))
    }
}

pub struct HotPrefixRandomAccessFile {
    prefix_len: u64,
    prefix_file: Option<Box<dyn RandomAccessFile>>,
    file: Box<dyn RandomAccessFile>,
}

impl RandomAccessFile for HotPrefixRandomAccessFile {
    fn read_at(&self, buf: &mut [u8], offset: u64, hint: ReadHint) -> io::Result<usize> {
        match self.prefix_file {
            Some(ref prefix_file) if offset < self.prefix_len => {
                prefix_file.read_at(buf, offset, hint)
            }
            _ => self.file.read_at(buf, offset, hint),
        }
    }
}
