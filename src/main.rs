#[macro_use]
mod errors;
mod antichess_tb;
mod filesystem;
mod metric;
mod op1;
mod prophet;
mod request;
mod response;
mod tablebases;

use std::{
    net::SocketAddr,
    path::PathBuf,
    sync::{
        Arc, atomic,
        atomic::{AtomicU64, AtomicUsize},
    },
    thread,
    time::Duration,
};

use axum::{
    Router,
    extract::{Path, Query, State},
    routing::get,
};
use axum_content_negotiation::{Negotiate, NegotiateLayer};
use clap::{ArgAction, CommandFactory as _, Parser, builder::PathBufValueParser};
use listenfd::ListenFd;
use moka::future::Cache;
use op1::Op1Client;
use tikv_jemallocator::Jemalloc;
use tokio::{
    net::{TcpListener, UnixListener},
    sync::{mpsc, oneshot},
};
use tower::ServiceBuilder;
use tower_http::trace::TraceLayer;
use tracing::{Instrument as _, info, info_span, trace};

use crate::{
    errors::TablebaseError,
    filesystem::{HotPrefixFilesystem, TokioUringFilesystem},
    request::{TablebaseQuery, TablebaseVariant},
    response::{MainlineResponse, TablebaseResponse},
    tablebases::Tablebases,
};

#[global_allocator]
static GLOBAL: Jemalloc = Jemalloc;

#[derive(Parser, Debug, Clone)]
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
    /// Directory with Prophet tablebase files.
    #[arg(long, action = ArgAction::Append, value_parser = PathBufValueParser::new())]
    prophet: Vec<PathBuf>,
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

    /// Number of io_uring backend threads to probe Syzygy tables on.
    #[arg(long, default_value = "4")]
    backends: usize,

    /// Maximum number of cached responses.
    #[arg(long, default_value = "20000")]
    cache: u64,

    /// Listen on this socket address.
    #[arg(long, default_value = "127.0.0.1:9000")]
    bind: SocketAddr,
}

struct AppState {
    backends: RoundRobin<TablebaseRequest>,
    cache: TablebaseCache,
    cache_miss: AtomicU64,
}

/// Dispatches requests to backend threads in round-robin order.
struct RoundRobin<T> {
    txs: Vec<mpsc::Sender<T>>,
    current: AtomicUsize,
}

impl<T> RoundRobin<T> {
    async fn send(&self, req: T) -> Result<(), mpsc::error::SendError<T>> {
        let i = self.current.fetch_add(1, atomic::Ordering::Relaxed) % self.txs.len();
        self.txs[i].send(req).await
    }
}

type TablebaseCache = Cache<(TablebaseVariant, TablebaseQuery), TablebaseResponse>;

/// A unit of work handed to a backend thread. The borrowed per-thread tablebase
/// handles (io_uring files) never leave the backend; only the `Send` result
/// travels back over the oneshot channel.
enum TablebaseRequest {
    Probe {
        variant: TablebaseVariant,
        query: TablebaseQuery,
        tx: oneshot::Sender<Result<TablebaseResponse, TablebaseError>>,
    },
    Mainline {
        variant: TablebaseVariant,
        query: TablebaseQuery,
        tx: oneshot::Sender<Result<MainlineResponse, TablebaseError>>,
    },
}

/// Spawns one backend thread running a dedicated `tokio-uring` runtime, with
/// its own set of (per-runtime) open table files.
fn spawn_backend(opt: Opt) -> mpsc::Sender<TablebaseRequest> {
    let (tx, mut rx) = mpsc::channel(256);
    thread::spawn(move || {
        tokio_uring::builder()
            .uring_builder(tokio_uring::uring_builder().setup_sqpoll(500))
            .entries(1024)
            .start(async move {
                // Prepare the custom Syzygy filesystem (io_uring + hot prefix).
                let mut filesystem = HotPrefixFilesystem::new(TokioUringFilesystem);
                for path in &opt.hot_prefix {
                    let n = filesystem
                        .add_prefix_directory(path)
                        .expect("add hot prefix directory");
                    info!("added {} hot prefix candidate files from {}", n, path.display());
                }

                // Initialize Syzygy tablebases for this runtime.
                let mut tbs = Tablebases::with_filesystem(filesystem);
                for path in &opt.standard {
                    let n = tbs.standard.add_directory(path).await.expect("standard dir");
                    info!("added {} standard tables from {}", n, path.display());
                }
                for path in &opt.atomic {
                    let n = tbs.atomic.add_directory(path).await.expect("atomic dir");
                    info!("added {} atomic tables from {}", n, path.display());
                }
                for path in &opt.antichess {
                    let n = tbs.antichess.add_directory(path).await.expect("antichess dir");
                    info!("added {} antichess tables from {}", n, path.display());
                }
                tbs.op1 = opt.op1_endpoint.as_deref().map(Op1Client::new);

                // Serve requests on this runtime; each probe runs as its own
                // task so successor probes within a request can be concurrent.
                let tbs: &'static Tablebases = Box::leak(Box::new(tbs));
                while let Some(req) = rx.recv().await {
                    match req {
                        TablebaseRequest::Probe { variant, query, tx } => {
                            tokio_uring::spawn(async move {
                                let TablebaseQuery { fen, dtc } = query;
                                let result = match variant.position(fen) {
                                    Ok(pos) => tbs.probe(pos, dtc).await,
                                    Err(err) => Err(err.into()),
                                };
                                let _ = tx.send(result);
                            });
                        }
                        TablebaseRequest::Mainline { variant, query, tx } => {
                            tokio_uring::spawn(async move {
                                let result = match variant.position(query.fen) {
                                    Ok(pos) => tbs.mainline(pos).await.map_err(TablebaseError::from),
                                    Err(err) => Err(err.into()),
                                };
                                let _ = tx.send(result);
                            });
                        }
                    }
                }
            });
    });
    tx
}

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
            let (tx, rx) = oneshot::channel();
            app.backends
                .send(TablebaseRequest::Probe { variant, query, tx })
                .await
                .expect("backend alive");
            rx.await
                .expect("backend answers request")
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

    async move {
        let (tx, rx) = oneshot::channel();
        app.backends
            .send(TablebaseRequest::Mainline { variant, query, tx })
            .await
            .expect("backend alive");
        rx.await
            .expect("backend answers request")
            .map(Negotiate)
            .inspect(|_| trace!("success"))
            .inspect_err(|error| dyn_event!(error.tracing_level(), %error, "fail"))
    }
    .instrument(span)
    .await
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
        && opt.prophet.is_empty()
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

    // Initialize the global (process-wide) FFI tablebases exactly once.
    unsafe {
        prophet::init(&opt.prophet);
        antichess_tb::init(&opt.antichess_tb);
    }

    // Spawn the io_uring backend threads. Each builds its own Syzygy tables.
    let backends = RoundRobin {
        txs: (0..opt.backends.max(1))
            .map(|_| spawn_backend(opt.clone()))
            .collect(),
        current: AtomicUsize::new(0),
    };

    let state: &'static AppState = Box::leak(Box::new(AppState {
        backends,
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
