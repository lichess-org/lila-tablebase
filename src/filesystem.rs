use std::{
    collections::HashMap,
    ffi::OsString,
    fs,
    fs::File,
    io,
    os::unix::{fs::FileExt as _, io::AsRawFd as _},
    path::{Path, PathBuf},
};

use shakmaty_syzygy::filesystem::{Filesystem, RandomAccessFile, ReadHint};

#[derive(Clone)]
pub struct HotPrefixFilesystem {
    prefix_candidates: HashMap<OsString, PathBuf>,
}

impl HotPrefixFilesystem {
    pub fn new() -> HotPrefixFilesystem {
        HotPrefixFilesystem {
            prefix_candidates: HashMap::new(),
        }
    }

    pub fn add_directory(&mut self, path: &Path) -> io::Result<usize> {
        let mut n = 0;
        for entry in fs::read_dir(path)? {
            let entry = entry?;
            let path = entry.path();
            if !path.extension().map_or(false, |ext| ext == "prefix") {
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
        let meta = path.metadata()?;
        if !meta.is_file() {
            return Err(io::Error::new(
                io::ErrorKind::InvalidInput,
                "not a regular file",
            ));
        }
        Ok(meta.len())
    }

    fn read_dir(&self, path: &Path) -> io::Result<Vec<PathBuf>> {
        fs::read_dir(path)?
            .map(|maybe_entry| maybe_entry.map(|entry| entry.path().to_owned()))
            .collect()
    }

    fn open(&self, path: &Path) -> io::Result<Box<dyn RandomAccessFile>> {
        let file = File::open(path)?;

        unsafe {
            libc::posix_fadvise(file.as_raw_fd(), 0, 0, libc::POSIX_FADV_RANDOM);
        }

        Ok(Box::new(
            if let Some(prefix_file_path) = path
                .file_name()
                .and_then(|name| self.prefix_candidates.get(name))
            {
                HotPrefixRandomAccessFile {
                    file,
                    prefix_file: Some(File::open(prefix_file_path)?),
                    prefix_len: prefix_file_path.metadata()?.len(),
                }
            } else {
                HotPrefixRandomAccessFile {
                    file,
                    prefix_file: None,
                    prefix_len: 0,
                }
            },
        ))
    }
}

pub struct HotPrefixRandomAccessFile {
    file: File,
    prefix_file: Option<File>,
    prefix_len: u64,
}

impl RandomAccessFile for HotPrefixRandomAccessFile {
    fn read_at(&self, buf: &mut [u8], offset: u64, _hint: ReadHint) -> io::Result<usize> {
        match self.prefix_file {
            Some(ref prefix_file) if offset < self.prefix_len => prefix_file.read_at(buf, offset),
            _ => self.file.read_at(buf, offset),
        }
    }
}
