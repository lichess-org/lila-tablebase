#![forbid(unsafe_op_in_unsafe_fn)]

#[macro_use]
mod errors;
mod filesystem;
mod gaviota;
mod request;
mod response;
mod tablebases;

use std::{
    net::SocketAddr,
    path::PathBuf,
    sync::{
        atomic,
        atomic::{AtomicBool, AtomicU64},
        Arc,
    },
    time::Duration,
};

use axum::{
    extract::{Path, Query, State},
    routing::get,
    Json, Router,
};
use clap::{builder::PathBufValueParser, ArgAction, CommandFactory as _, Parser};
use listenfd::ListenFd;
use moka::future::Cache;
use shakmaty_syzygy::filesystem::{MmapFilesystem, OsFilesystem};
use tokio::{net::TcpListener, sync::Semaphore, task};
use tower_http::trace::TraceLayer;
use tracing::{info, info_span, trace, Instrument as _};

use crate::{
    errors::TablebaseError,
    filesystem::HotPrefixFilesystem,
    request::{TablebaseQuery, TablebaseVariant},
    response::{MainlineResponse, TablebaseResponse},
    tablebases::Tablebases,
};

#[derive(Parser, Debug)]
struct Opt {
    /// Directory with tablebase files for standard chess.
    #[arg(long, action = ArgAction::Append, value_parser = PathBufValueParser::new())]
    standard: Vec<PathBuf>,
    /// Directory with tablebase files for atomic chess.
    #[arg(long, action = ArgAction::Append, value_parser = PathBufValueParser::new())]
    atomic: Vec<PathBuf>,
    /// Directory with tablebase files for antichess.
    #[arg(long, action = ArgAction::Append, value_parser = PathBufValueParser::new())]
    antichess: Vec<PathBuf>,
    /// Directory with Gaviota tablebase files.
    #[arg(long, action = ArgAction::Append, value_parser = PathBufValueParser::new())]
    gaviota: Vec<PathBuf>,

    /// Directory with prefix files.
    ///
    /// For a table named KRvK.rtbw, a corresponding prefix file named
    /// KRvK.rtbw.prefix will be used if present. It can contain any number of
    /// bytes from the start of the original table file. Storing the prefix
    /// file on a faster medium may speed up acccess to the sparse block index
    /// and the block length table.
    #[arg(long, action = ArgAction::Append, value_parser = PathBufValueParser::new())]
    hot_prefix: Vec<PathBuf>,

    /// Use memory maps to read table files.
    ///
    /// May crash on I/O errors, and cause undefined behavior when open table
    /// files are modified.
    #[arg(long)]
    mmap: bool,

    /// Do not set POSIX_FADVISE_RANDOM or MADV_RANDOM on table files.
    #[arg(long)]
    no_advise_random: bool,

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
    semaphore: Semaphore,
    deploy_event_sent: AtomicBool,
}

type TablebaseCache = Cache<(TablebaseVariant, TablebaseQuery), TablebaseResponse>;

fn try_probe(
    tbs: &Tablebases,
    variant: TablebaseVariant,
    query: TablebaseQuery,
) -> Result<TablebaseResponse, TablebaseError> {
    Ok(tbs.probe(&variant.position(query.fen)?)?)
}

async fn handle_probe(
    State(app): State<&'static AppState>,
    Path(variant): Path<TablebaseVariant>,
    Query(query): Query<TablebaseQuery>,
) -> Result<Json<TablebaseResponse>, TablebaseError> {
    let pieces = query.fen.0.board.occupied().count();
    let span = match variant {
        TablebaseVariant::Standard => info_span!("standard request", fen = %query.fen, pieces),
        TablebaseVariant::Atomic => info_span!("atomic request", fen = %query.fen, pieces),
        TablebaseVariant::Antichess => info_span!("antichess request", fen = %query.fen, pieces),
    };

    let blocking_span = span.clone();
    async move {
        info!("begin");
        app.cache
            .try_get_with((variant, query.clone()), async move {
                app.cache_miss.fetch_add(1, atomic::Ordering::Relaxed);
                let _permit = app.semaphore.acquire().await.expect("semaphore not closed");
                task::spawn_blocking(move || {
                    blocking_span.in_scope(|| try_probe(&app.tbs, variant, query))
                })
                .await
                .expect("blocking probe")
            })
            .await
            .map(Json)
            .map_err(Arc::unwrap_or_clone)
            .inspect(|_| trace!("success"))
            .inspect_err(|error| dyn_event!(error.tracing_level(), %error, "fail"))
    }
    .instrument(span)
    .await
}

fn try_mainline(
    tbs: &Tablebases,
    variant: TablebaseVariant,
    query: TablebaseQuery,
) -> Result<MainlineResponse, TablebaseError> {
    Ok(tbs.mainline(variant.position(query.fen)?)?)
}

async fn handle_mainline(
    State(app): State<&'static AppState>,
    Path(variant): Path<TablebaseVariant>,
    Query(query): Query<TablebaseQuery>,
) -> Result<Json<MainlineResponse>, TablebaseError> {
    let span = match variant {
        TablebaseVariant::Standard => info_span!("standard mainline request", fen = %query.fen),
        TablebaseVariant::Atomic => info_span!("atomic mainline request", fen = %query.fen),
        TablebaseVariant::Antichess => info_span!("antichess mainline request", fen = %query.fen),
    };

    let blocking_span = span.clone();
    async move {
        info!("begin");
        let _permit = app.semaphore.acquire().await.expect("semaphore not closed");
        task::spawn_blocking(move || {
            blocking_span.in_scope(|| try_mainline(&app.tbs, variant, query))
        })
        .await
        .expect("blocking mainline")
        .map(Json)
        .inspect(|_| trace!("success"))
        .inspect_err(|error| dyn_event!(error.tracing_level(), %error, "fail"))
    }
    .instrument(span)
    .await
}

async fn handle_monitor(State(app): State<&'static AppState>) -> String {
    if app
        .deploy_event_sent
        .fetch_or(true, atomic::Ordering::Relaxed)
    {
        let cache = app.cache.entry_count();
        let cache_miss = app.cache_miss.load(atomic::Ordering::Relaxed);
        let metrics = &[
            format!("cache={cache}u"),
            format!("cache_miss={cache_miss}u"),
        ];
        format!("tablebase {}", metrics.join(","))
    } else {
        format!(
            "event,program=lila-tablebase commit={:?},text={:?}",
            env!("VERGEN_GIT_SHA"),
            env!("VERGEN_GIT_COMMIT_MESSAGE")
        )
    }
}

async fn serve(opt: Opt) {
    let state: &'static AppState = Box::leak(Box::new(AppState {
        tbs: {
            // Initialize Gaviota tablebase.
            if !opt.gaviota.is_empty() {
                unsafe {
                    gaviota::init(&opt.gaviota);
                }
            }

            // Prepare custom Syzygy filesystem implementation.
            let mut filesystem = HotPrefixFilesystem::new(if opt.mmap {
                unsafe { Box::new(MmapFilesystem::new().with_advise_random(!opt.no_advise_random)) }
            } else {
                Box::new(OsFilesystem::new().with_advise_random(!opt.no_advise_random))
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
            tbs
        },
        cache: Cache::builder()
            .max_capacity(opt.cache)
            .time_to_idle(Duration::from_secs(60 * 5))
            .build(),
        cache_miss: AtomicU64::new(0),
        semaphore: Semaphore::new(128),
        deploy_event_sent: AtomicBool::new(false),
    }));

    let app = Router::new()
        .route("/monitor", get(handle_monitor))
        .route("/:variant", get(handle_probe))
        .route("/:variant/mainline", get(handle_mainline))
        .with_state(state)
        .layer(TraceLayer::new_for_http());

    let listener = match ListenFd::from_env()
        .take_tcp_listener(0)
        .expect("tcp listener")
    {
        Some(std_listener) => {
            std_listener.set_nonblocking(true).expect("set nonblocking");
            TcpListener::from_std(std_listener).expect("listener")
        }
        None => TcpListener::bind(&opt.bind).await.expect("bind"),
    };

    axum::serve(listener, app).await.expect("serve");
}

fn main() {
    // Parse arguments.
    let opt = Opt::parse();
    if opt.standard.is_empty()
        && opt.atomic.is_empty()
        && opt.antichess.is_empty()
        && opt.gaviota.is_empty()
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

    // Start async runtime.
    tokio::runtime::Builder::new_multi_thread()
        .enable_all()
        .max_blocking_threads(128)
        .build()
        .expect("tokio runtime")
        .block_on(serve(opt));
}
