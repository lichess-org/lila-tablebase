#![forbid(unsafe_op_in_unsafe_fn)]

#[macro_use]
mod errors;
mod antichess_tb;
mod filesystem;
mod gaviota;
mod op1;
mod request;
mod response;
mod tablebases;

use std::{
    net::SocketAddr,
    path::PathBuf,
    sync::{atomic, atomic::AtomicU64, Arc},
    time::Duration,
};

use axum::{
    extract::{Path, Query, State},
    routing::get,
    Router,
};
use axum_content_negotiation::{Negotiate, NegotiateLayer};
use clap::{builder::PathBufValueParser, ArgAction, CommandFactory as _, Parser};
use listenfd::ListenFd;
use moka::future::Cache;
use op1::Op1Client;
use shakmaty_syzygy::filesystem::{MmapFilesystem, OsFilesystem};
use tikv_jemallocator::Jemalloc;
use tokio::net::{TcpListener, UnixListener};
use tower::ServiceBuilder;
use tower_http::trace::TraceLayer;
use tracing::{info, info_span, trace, Instrument as _};

use crate::{
    errors::TablebaseError,
    filesystem::HotPrefixFilesystem,
    request::{TablebaseQuery, TablebaseVariant},
    response::{MainlineResponse, TablebaseResponse},
    tablebases::Tablebases,
};

#[global_allocator]
static GLOBAL: Jemalloc = Jemalloc;

#[derive(Parser, Debug)]
struct Opt {
    /// Directory with Syzygy tablebase files for standard chess.
    #[arg(long, action = ArgAction::Append, value_parser = PathBufValueParser::new())]
    standard: Vec<PathBuf>,
    /// Directory with Syzygy tablebase files for atomic chess.
    #[arg(long, action = ArgAction::Append, value_parser = PathBufValueParser::new())]
    atomic: Vec<PathBuf>,
    /// Directory with Syzygy tablebase files for antichess.
    #[arg(long, action = ArgAction::Append, value_parser = PathBufValueParser::new())]
    antichess: Vec<PathBuf>,
    /// Directory with DTW tablebase files for antichess.
    #[arg(long, action = ArgAction::Append, value_parser = PathBufValueParser::new())]
    antichess_tb: Vec<PathBuf>,
    /// Directory with Gaviota tablebase files.
    #[arg(long, action = ArgAction::Append, value_parser = PathBufValueParser::new())]
    gaviota: Vec<PathBuf>,
    // Op1 server.
    #[arg(long)]
    op1_endpoint: Option<String>,

    /// Directory with prefix files.
    ///
    /// For example, for a table named KRvK.rtbw, a corresponding prefix file
    /// named KRvK.rtbw.prefix would be used if present. It can contain any
    /// number of bytes from the start of the original table file.
    ///
    /// This is particularly useful to speed up access to the table header,
    /// sparse block index, and block length table, by storing them on
    /// a faster medium.
    #[arg(long, action = ArgAction::Append, value_parser = PathBufValueParser::new())]
    hot_prefix: Vec<PathBuf>,

    /// Use memory maps to read table files.
    ///
    /// May crash on I/O errors, and cause undefined behavior when open table
    /// files are modified. May harm performance when using HDDs.
    #[arg(long)]
    mmap: bool,

    /// Set POSIX_FADVISE_RANDOM or MADV_RANDOM on table files.
    ///
    /// Use only after benchmarking the specific setup. Performance may be
    /// harmed or improved.
    #[arg(long)]
    advise_random: bool,

    /// Maximum number of cached responses.
    #[arg(long, default_value = "20000")]
    cache: u64,

    /// Listen on this socket address.
    #[arg(long, default_value = "127.0.0.1:9000")]
    bind: SocketAddr,
}

struct AppState {
    tbs: Tablebases,
    cache: TablebaseCache,
    cache_miss: AtomicU64,
}

type TablebaseCache = Cache<(TablebaseVariant, TablebaseQuery), TablebaseResponse>;

async fn handle_probe(
    State(app): State<&'static AppState>,
    Path(variant): Path<TablebaseVariant>,
    Query(query): Query<TablebaseQuery>,
) -> Result<Negotiate<TablebaseResponse>, TablebaseError> {
    let pieces = query.fen.as_setup().board.occupied().count();
    let span = match variant {
        TablebaseVariant::Standard => info_span!("standard request", fen = %query.fen, pieces),
        TablebaseVariant::Atomic => info_span!("atomic request", fen = %query.fen, pieces),
        TablebaseVariant::Antichess => info_span!("antichess request", fen = %query.fen, pieces),
    };

    app.cache
        .try_get_with((variant, query.clone()), async move {
            app.cache_miss.fetch_add(1, atomic::Ordering::Relaxed);
            app.tbs
                .probe(variant.position(query.fen)?, query.op1)
                .await
                .map_err(TablebaseError::from)
                .inspect(|_| trace!("success"))
                .inspect_err(|error| dyn_event!(error.tracing_level(), %error, "fail"))
        })
        .instrument(span)
        .await
        .map(Negotiate)
        .map_err(Arc::unwrap_or_clone)
}

async fn handle_mainline(
    State(app): State<&'static AppState>,
    Path(variant): Path<TablebaseVariant>,
    Query(query): Query<TablebaseQuery>,
) -> Result<Negotiate<MainlineResponse>, TablebaseError> {
    let span = match variant {
        TablebaseVariant::Standard => info_span!("standard mainline request", fen = %query.fen),
        TablebaseVariant::Atomic => info_span!("atomic mainline request", fen = %query.fen),
        TablebaseVariant::Antichess => info_span!("antichess mainline request", fen = %query.fen),
    };

    app.tbs
        .mainline(variant.position(query.fen)?)
        .instrument(span)
        .await
        .map(Negotiate)
        .map_err(TablebaseError::from)
        .inspect(|_| trace!("success"))
        .inspect_err(|error| dyn_event!(error.tracing_level(), %error, "fail"))
}

async fn handle_monitor(State(app): State<&'static AppState>) -> String {
    let cache = app.cache.entry_count();
    let cache_miss = app.cache_miss.load(atomic::Ordering::Relaxed);
    let metrics = &[
        format!("cache={cache}u"),
        format!("cache_miss={cache_miss}u"),
    ];
    format!("tablebase {}", metrics.join(","))
}

#[tokio::main]
async fn main() {
    // Parse arguments.
    let opt = Opt::parse();
    if opt.standard.is_empty()
        && opt.atomic.is_empty()
        && opt.antichess.is_empty()
        && opt.gaviota.is_empty()
        && opt.op1_endpoint.is_none()
    {
        Opt::command().print_help().expect("usage");
        println!();
        return;
    }

    // Prepare tracing.
    tracing_subscriber::fmt()
        .event_format(tracing_subscriber::fmt::format().compact())
        .without_time()
        .with_env_filter(tracing_subscriber::EnvFilter::from_default_env())
        .init();

    let state: &'static AppState = Box::leak(Box::new(AppState {
        tbs: {
            // Initialize Gaviota tablebase.
            unsafe {
                gaviota::init(&opt.gaviota);
            }

            // Initialize Antichess tablebase.
            unsafe {
                antichess_tb::init(&opt.antichess_tb);
            }

            // Prepare custom Syzygy filesystem implementation.
            let mut filesystem = HotPrefixFilesystem::new(if opt.mmap {
                unsafe { Box::new(MmapFilesystem::new().with_advise_random(opt.advise_random)) }
            } else {
                Box::new(OsFilesystem::new().with_advise_random(opt.advise_random))
            });
            for path in opt.hot_prefix {
                let n = filesystem
                    .add_prefix_directory(&path)
                    .expect("add hot prefix directory");
                info!(
                    "added {} hot prefix candidate files from {}",
                    n,
                    path.display()
                );
            }

            // Initialize Syzygy tablebases.
            let mut tbs = Tablebases::with_filesystem(Arc::new(filesystem));
            for path in opt.standard {
                let n = tbs
                    .standard
                    .add_directory(&path)
                    .expect("open standard directory");
                info!("added {} standard tables from {}", n, path.display());
            }
            for path in opt.atomic {
                let n = tbs
                    .atomic
                    .add_directory(&path)
                    .expect("open atomic directory");
                info!("added {} atomic tables from {}", n, path.display());
            }
            for path in opt.antichess {
                let n = tbs
                    .antichess
                    .add_directory(&path)
                    .expect("open antichess directory");
                info!("added {} antichess tables from {}", n, path.display());
            }

            // Initialize OP1
            tbs.op1 = opt.op1_endpoint.map(|endpoint| Op1Client::new(&endpoint));

            tbs
        },
        cache: Cache::builder()
            .max_capacity(opt.cache)
            .time_to_idle(Duration::from_secs(60 * 10))
            .build(),
        cache_miss: AtomicU64::new(0),
    }));

    let app = Router::new()
        .route("/monitor", get(handle_monitor))
        .route("/{variant}", get(handle_probe))
        .route("/{variant}/mainline", get(handle_mainline))
        .with_state(state)
        .layer(
            ServiceBuilder::new()
                .layer(TraceLayer::new_for_http())
                .layer(NegotiateLayer),
        );

    let mut fds = ListenFd::from_env();
    if let Ok(Some(uds)) = fds.take_unix_listener(0) {
        uds.set_nonblocking(true).expect("set nonblocking");
        let listener = UnixListener::from_std(uds).expect("listener");
        axum::serve(listener, app).await.expect("serve");
    } else if let Ok(Some(tcp)) = fds.take_tcp_listener(0) {
        tcp.set_nonblocking(true).expect("set nonblocking");
        let listener = TcpListener::from_std(tcp).expect("listener");
        axum::serve(listener, app).await.expect("serve");
    } else {
        let listener = TcpListener::bind(&opt.bind).await.expect("bind");
        axum::serve(listener, app).await.expect("serve");
    }
}
