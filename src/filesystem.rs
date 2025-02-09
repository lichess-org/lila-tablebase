use std::{
    collections::HashMap,
    ffi::OsString,
    fs, io,
    os::unix::fs::FileExt,
    path::{Path, PathBuf},
    sync::Arc,
};

use shakmaty_syzygy::aio::{Filesystem, RandomAccessFile, ReadHint};
use tokio::task;

#[derive(Clone)]
pub struct TokioFilesystem;

impl Filesystem for TokioFilesystem {
    type RandomAccessFile = TokioRandomAccessFile;

    async fn read_dir(&self, path: &Path) -> io::Result<Vec<PathBuf>> {
        let mut paths = Vec::new();
        let mut read_dir = tokio::fs::read_dir(path).await?;
        while let Some(entry) = read_dir.next_entry().await? {
            paths.push(entry.path());
        }
        Ok(paths)
    }

    async fn regular_file_size(&self, path: &Path) -> io::Result<u64> {
        let meta = tokio::fs::metadata(path).await?;
        if !meta.is_file() {
            return Err(io::Error::new(
                io::ErrorKind::InvalidInput,
                "not a regular file",
            ));
        }
        Ok(meta.len())
    }

    async fn open(&self, path: &Path) -> io::Result<TokioRandomAccessFile> {
        let path = path.to_owned();
        task::spawn_blocking(|| {
            Ok(TokioRandomAccessFile {
                file: Arc::new(fs::File::open(path)?),
            })
        })
        .await
        .expect("blocking open")
    }
}

pub struct TokioRandomAccessFile {
    file: Arc<fs::File>,
}

impl RandomAccessFile for TokioRandomAccessFile {
    async fn read_at(&self, buf: &mut [u8], offset: u64, _hint: ReadHint) -> io::Result<usize> {
        let max_len = buf.len();
        let file = Arc::clone(&self.file);
        let owned_buf = task::spawn_blocking(move || {
            let mut owned_buf = vec![0; max_len];
            let n = file.read_at(&mut owned_buf[..], offset)?;
            owned_buf.truncate(n);
            Ok::<_, io::Error>(owned_buf)
        })
        .await
        .expect("blocking read_at")?;

        buf[..owned_buf.len()].copy_from_slice(&owned_buf);
        Ok(owned_buf.len())
    }

    async fn read_exact_at(&self, buf: &mut [u8], offset: u64, _hint: ReadHint) -> io::Result<()> {
        let len = buf.len();
        let file = Arc::clone(&self.file);
        let owned_buf = task::spawn_blocking(move || {
            let mut owned_buf = vec![0; len];
            file.read_exact_at(&mut owned_buf[..], offset)?;
            Ok::<_, io::Error>(owned_buf)
        })
        .await
        .expect("blocking read_exact_at")?;

        buf.copy_from_slice(&owned_buf);
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
