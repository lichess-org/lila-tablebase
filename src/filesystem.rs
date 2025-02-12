use std::{
    collections::HashMap,
    ffi::OsString,
    fs, io, iter,
    path::{Path, PathBuf},
};

use shakmaty_syzygy::aio::{Filesystem, RandomAccessFile, ReadHint};
use tokio_uring::buf::fixed::FixedBufPool;

#[derive(Clone)]
pub struct TokioUringFilesystem {
    buf_pool: FixedBufPool<Vec<u8>>,
}

impl TokioUringFilesystem {
    pub fn new() -> TokioUringFilesystem {
        let buf_pool = FixedBufPool::new(
            iter::empty()
                .chain(iter::repeat_with(|| Vec::with_capacity(1)).take(10))
                .chain(iter::repeat_with(|| Vec::with_capacity(2)).take(10))
                .chain(iter::repeat_with(|| Vec::with_capacity(3)).take(10))
                .chain(iter::repeat_with(|| Vec::with_capacity(4)).take(10))
                .chain(iter::repeat_with(|| Vec::with_capacity(5)).take(10))
                .chain(iter::repeat_with(|| Vec::with_capacity(6)).take(60)) // sparse index
                .chain(iter::repeat_with(|| Vec::with_capacity(7)).take(10))
                .chain(iter::repeat_with(|| Vec::with_capacity(10)).take(10))
                .chain(iter::repeat_with(|| Vec::with_capacity((1 << 0) + 4)).take(50))
                .chain(iter::repeat_with(|| Vec::with_capacity((1 << 1) + 4)).take(50))
                .chain(iter::repeat_with(|| Vec::with_capacity((1 << 2) + 4)).take(50))
                .chain(iter::repeat_with(|| Vec::with_capacity((1 << 3) + 4)).take(50))
                .chain(iter::repeat_with(|| Vec::with_capacity((1 << 4) + 4)).take(50))
                .chain(iter::repeat_with(|| Vec::with_capacity((1 << 5) + 4)).take(50))
                .chain(iter::repeat_with(|| Vec::with_capacity((1 << 6) + 4)).take(50))
                .chain(iter::repeat_with(|| Vec::with_capacity((1 << 7) + 4)).take(50))
                .chain(iter::repeat_with(|| Vec::with_capacity((1 << 8) + 4)).take(50))
                .chain(iter::repeat_with(|| Vec::with_capacity((1 << 9) + 4)).take(50))
                .chain(iter::repeat_with(|| Vec::with_capacity((1 << 10) + 4)).take(50)),
        );
        buf_pool
            .register()
            .expect("register buf pool in current ring");
        TokioUringFilesystem { buf_pool }
    }
}

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
            buf_pool: self.buf_pool.clone(),
        })
    }
}

pub struct TokioUringRandomAccessFile {
    file: tokio_uring::fs::File,
    buf_pool: FixedBufPool<Vec<u8>>,
}

impl RandomAccessFile for TokioUringRandomAccessFile {
    async fn read_at(&self, _buf: &mut [u8], _offset: u64, _hint: ReadHint) -> io::Result<usize> {
        unreachable!("always using read_exact_at")
    }

    async fn read_exact_at(&self, buf: &mut [u8], offset: u64, _hint: ReadHint) -> io::Result<()> {
        let owned_buf = self.buf_pool.next(buf.len()).await;
        let (res, owned_buf) = self.file.read_fixed_at(owned_buf, offset).await;
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
