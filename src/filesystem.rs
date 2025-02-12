use std::{
    collections::HashMap,
    ffi::OsString,
    fs, io,
    path::{Path, PathBuf},
};

use shakmaty_syzygy::aio::{Filesystem, RandomAccessFile, ReadHint};

#[derive(Clone)]
pub struct TokioUringFilesystem;

impl Filesystem for TokioUringFilesystem {
    type RandomAccessFile = TokioUringRandomAccessFile;

    async fn read_dir(&self, path: &Path) -> io::Result<Vec<PathBuf>> {
        fs::read_dir(path)?
            .map(|maybe_entry| maybe_entry.map(|entry| entry.path().to_owned()))
            .collect()
    }

    async fn regular_file_size(&self, path: &Path) -> io::Result<u64> {
        let stat = tokio_uring::fs::statx(path).await?;
        if libc::mode_t::from(stat.stx_mode) & libc::S_IFMT != libc::S_IFREG {
            return Err(io::Error::new(
                io::ErrorKind::InvalidInput,
                "not a regular file",
            ));
        }
        Ok(stat.stx_size)
    }

    async fn open(&self, path: &Path) -> io::Result<TokioUringRandomAccessFile> {
        Ok(TokioUringRandomAccessFile {
            file: tokio_uring::fs::File::open(path).await?,
        })
    }
}

pub struct TokioUringRandomAccessFile {
    file: tokio_uring::fs::File,
}

impl RandomAccessFile for TokioUringRandomAccessFile {
    async fn read_at(&self, buf: &mut [u8], offset: u64, _hint: ReadHint) -> io::Result<usize> {
        let owned_buf = vec![0; buf.len()];
        let (res, owned_buf) = self.file.read_at(owned_buf, offset).await;
        let n = res?;
        buf[..n].copy_from_slice(&owned_buf[..n]);
        Ok(n)
    }

    async fn read_exact_at(&self, buf: &mut [u8], offset: u64, _hint: ReadHint) -> io::Result<()> {
        let owned_buf = Vec::with_capacity(buf.len());
        let (res, owned_buf) = self.file.read_exact_at(owned_buf, offset).await;
        res?;
        buf[..].copy_from_slice(&owned_buf);
        Ok(())
    }
}

#[derive(Clone)]
pub struct HotPrefixFilesystem<F> {
    underlying: F,
    prefix_candidates: HashMap<OsString, PathBuf>,
}

impl<F> HotPrefixFilesystem<F> {
    pub fn new(underlying: F) -> HotPrefixFilesystem<F> {
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

impl<F: Filesystem> Filesystem for HotPrefixFilesystem<F> {
    type RandomAccessFile = HotPrefixRandomAccessFile<F::RandomAccessFile>;

    async fn read_dir(&self, path: &Path) -> io::Result<Vec<PathBuf>> {
        self.underlying.read_dir(path).await
    }

    async fn regular_file_size(&self, path: &Path) -> io::Result<u64> {
        self.underlying.regular_file_size(path).await
    }

    async fn open(&self, path: &Path) -> io::Result<Self::RandomAccessFile> {
        Ok(
            if let Some(prefix_file_path) = path
                .file_name()
                .and_then(|name| self.prefix_candidates.get(name))
            {
                HotPrefixRandomAccessFile {
                    prefix_len: self.regular_file_size(prefix_file_path).await?,
                    prefix_file: Some(self.underlying.open(prefix_file_path).await?),
                    file: self.underlying.open(path).await?,
                }
            } else {
                HotPrefixRandomAccessFile {
                    prefix_len: 0,
                    prefix_file: None,
                    file: self.underlying.open(path).await?,
                }
            },
        )
    }
}

pub struct HotPrefixRandomAccessFile<R> {
    prefix_len: u64,
    prefix_file: Option<R>,
    file: R,
}

impl<R: RandomAccessFile> RandomAccessFile for HotPrefixRandomAccessFile<R> {
    async fn read_at(&self, buf: &mut [u8], offset: u64, hint: ReadHint) -> io::Result<usize> {
        match self.prefix_file {
            Some(ref prefix_file) if offset + buf.len() as u64 <= self.prefix_len => {
                prefix_file.read_at(buf, offset, hint).await
            }
            _ => self.file.read_at(buf, offset, hint).await,
        }
    }

    async fn read_exact_at(&self, buf: &mut [u8], offset: u64, hint: ReadHint) -> io::Result<()> {
        match self.prefix_file {
            Some(ref prefix_file) if offset + buf.len() as u64 <= self.prefix_len => {
                prefix_file.read_exact_at(buf, offset, hint).await
            }
            _ => self.file.read_exact_at(buf, offset, hint).await,
        }
    }
}
