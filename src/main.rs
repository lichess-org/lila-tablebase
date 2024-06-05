#![forbid(unsafe_op_in_unsafe_fn)]

use std::{
    cmp::{Ordering, Reverse},
    ffi::CString,
    net::SocketAddr,
    ops::Neg,
    os::raw::{c_int, c_uchar, c_uint},
    path::PathBuf,
    ptr,
    sync::{
        atomic,
        atomic::{AtomicBool, AtomicU64},
        Arc,
    },
    time::Duration,
};

use arrayvec::ArrayVec;
use axum::{
    extract::{Path, Query, State},
    http::StatusCode,
    response::{IntoResponse, Response},
    routing::get,
    Json, Router,
};
use clap::{builder::PathBufValueParser, ArgAction, CommandFactory as _, Parser};
use listenfd::ListenFd;
use moka::future::Cache;
use serde::{Deserialize, Serialize};
use serde_with::{serde_as, DisplayFromStr, FromInto};
use shakmaty::{
    fen::{Fen, ParseFenError},
    san::SanPlus,
    uci::Uci,
    variant::{Antichess, Atomic, Chess, Variant, VariantPosition},
    CastlingMode, EnPassantMode, Move, Outcome, Position, PositionError, Role,
};
use shakmaty_syzygy::{AmbiguousWdl, Dtz, MaybeRounded, SyzygyError, Tablebase as SyzygyTablebase};
use tokio::{net::TcpListener, sync::Semaphore, task};
use tower_http::trace::TraceLayer;
use tracing::{error, info, warn};
use tracing_timing::TimingSubscriber;

#[derive(Deserialize, Debug, Copy, Clone, Eq, PartialEq, Hash)]
#[serde(rename_all = "lowercase")]
enum TablebaseVariant {
    Standard,
    Atomic,
    Antichess,
}

impl From<TablebaseVariant> for Variant {
    fn from(variant: TablebaseVariant) -> Variant {
        match variant {
            TablebaseVariant::Standard => Variant::Chess,
            TablebaseVariant::Atomic => Variant::Atomic,
            TablebaseVariant::Antichess => Variant::Antichess,
        }
    }
}

impl TablebaseVariant {
    fn position(self, fen: Fen) -> Result<VariantPosition, Arc<PositionError<VariantPosition>>> {
        VariantPosition::from_setup(self.into(), fen.into_setup(), CastlingMode::Standard)
            .or_else(PositionError::ignore_invalid_castling_rights)
            .or_else(PositionError::ignore_invalid_ep_square)
            .or_else(PositionError::ignore_impossible_check)
            .map_err(Arc::new)
    }
}

#[derive(Serialize, Debug, Clone)]
struct TablebaseResponse {
    #[serde(flatten)]
    pos: PositionInfo,
    category: Category,
    moves: ArrayVec<MoveInfo, 256>,
}

#[serde_as]
#[derive(Serialize, Debug, Clone)]
struct MoveInfo {
    #[serde_as(as = "DisplayFromStr")]
    uci: Uci,
    #[serde_as(as = "DisplayFromStr")]
    san: SanPlus,
    zeroing: bool,
    #[serde(skip)]
    capture: Option<Role>,
    #[serde(skip)]
    promotion: Option<Role>,
    #[serde(flatten)]
    pos: PositionInfo,
    category: Category,
}

#[serde_as]
#[derive(Serialize, Debug, Clone)]
struct PositionInfo {
    checkmate: bool,
    stalemate: bool,
    variant_win: bool,
    variant_loss: bool,
    insufficient_material: bool,
    #[serde_as(as = "Option<FromInto<i32>>")]
    #[serde(rename = "dtz")]
    maybe_rounded_dtz: Option<Dtz>,
    #[serde_as(as = "Option<FromInto<i32>>")]
    precise_dtz: Option<Dtz>,
    #[serde(skip)]
    dtz: Option<MaybeRounded<Dtz>>,
    dtm: Option<i32>,
    #[serde(skip)]
    halfmoves: u32,
}

impl PositionInfo {
    fn category(&self, halfmoves_before: u32) -> Category {
        if !self.variant_win
            && !self.variant_loss
            && (self.stalemate
                || self.insufficient_material
                || self.dtz.map_or(false, |dtz| dtz.is_zero()))
        {
            Category::Draw
        } else if self.checkmate || self.variant_loss {
            if halfmoves_before < 100 {
                Category::Loss
            } else {
                Category::BlessedLoss
            }
        } else if self.variant_win {
            if halfmoves_before < 100 {
                Category::Win
            } else {
                Category::CursedWin
            }
        } else if let Some(dtz) = self.dtz {
            if halfmoves_before < 100 {
                match AmbiguousWdl::from_dtz_and_halfmoves(dtz, self.halfmoves) {
                    AmbiguousWdl::Win => Category::Win,
                    AmbiguousWdl::MaybeWin => Category::MaybeWin,
                    AmbiguousWdl::CursedWin => Category::CursedWin,
                    AmbiguousWdl::Draw => Category::Draw,
                    AmbiguousWdl::BlessedLoss => Category::BlessedLoss,
                    AmbiguousWdl::MaybeLoss => Category::MaybeLoss,
                    AmbiguousWdl::Loss => Category::Loss,
                }
            } else if dtz.is_negative() {
                Category::BlessedLoss
            } else {
                Category::CursedWin
            }
        } else {
            Category::Unknown
        }
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Serialize)]
#[serde(rename_all = "kebab-case")]
enum Category {
    Loss,
    Unknown,
    MaybeLoss,
    BlessedLoss,
    Draw,
    CursedWin,
    MaybeWin,
    Win,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
struct PessimisticUnknown(Category); // loss < unknown < maybe-loss < ...

impl Ord for PessimisticUnknown {
    fn cmp(&self, other: &PessimisticUnknown) -> Ordering {
        (self.0 as u32).cmp(&(other.0 as u32))
    }
}

impl PartialOrd for PessimisticUnknown {
    fn partial_cmp(&self, other: &PessimisticUnknown) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Neg for Category {
    type Output = Category;

    fn neg(self) -> Category {
        match self {
            Category::Loss => Category::Win,
            Category::Unknown => Category::Unknown,
            Category::MaybeLoss => Category::MaybeWin,
            Category::BlessedLoss => Category::CursedWin,
            Category::Draw => Category::Draw,
            Category::CursedWin => Category::BlessedLoss,
            Category::MaybeWin => Category::MaybeLoss,
            Category::Win => Category::Loss,
        }
    }
}

#[serde_as]
#[derive(Serialize, Debug)]
struct MainlineResponse {
    mainline: Vec<MainlineStep>,
    winner: Option<char>,
    #[serde_as(as = "FromInto<i32>")]
    dtz: Dtz,
    #[serde_as(as = "Option<FromInto<i32>>")]
    precise_dtz: Option<Dtz>,
}

#[serde_as]
#[derive(Serialize, Debug)]
struct MainlineStep {
    #[serde_as(as = "DisplayFromStr")]
    uci: Uci,
    #[serde_as(as = "DisplayFromStr")]
    san: SanPlus,
    #[serde_as(as = "FromInto<i32>")]
    dtz: Dtz,
    #[serde_as(as = "Option<FromInto<i32>>")]
    precise_dtz: Option<Dtz>,
}

unsafe fn probe_dtm(pos: &VariantPosition) -> Option<i32> {
    let VariantPosition::Chess(ref pos) = pos else {
        return None;
    };

    if pos.board().occupied().count() > 5 || pos.castles().any() {
        return None;
    }

    if unsafe { gaviota_sys::tb_is_initialized() } == 0 {
        return None;
    }

    let mut ws = ArrayVec::<c_uint, 6>::new();
    let mut bs = ArrayVec::<c_uint, 6>::new();
    let mut wp = ArrayVec::<c_uchar, 6>::new();
    let mut bp = ArrayVec::<c_uchar, 6>::new();

    for (sq, piece) in pos.board().clone() {
        piece.color.fold_wb(&mut ws, &mut bs).push(c_uint::from(sq));
        piece
            .color
            .fold_wb(&mut wp, &mut bp)
            .push(c_uchar::from(piece.role));
    }

    ws.push(gaviota_sys::TB_squares::tb_NOSQUARE as c_uint);
    bs.push(gaviota_sys::TB_squares::tb_NOSQUARE as c_uint);
    wp.push(gaviota_sys::TB_pieces::tb_NOPIECE as c_uchar);
    bp.push(gaviota_sys::TB_pieces::tb_NOPIECE as c_uchar);

    let mut info: c_uint = 0;
    let mut plies: c_uint = 0;

    let result = unsafe {
        gaviota_sys::tb_probe_hard(
            pos.turn().fold_wb(
                gaviota_sys::TB_sides::tb_WHITE_TO_MOVE,
                gaviota_sys::TB_sides::tb_BLACK_TO_MOVE,
            ) as c_uint,
            pos.ep_square(EnPassantMode::Legal)
                .map_or(gaviota_sys::TB_squares::tb_NOSQUARE as c_uint, c_uint::from),
            gaviota_sys::TB_castling::tb_NOCASTLE.0,
            ws.as_ptr(),
            bs.as_ptr(),
            wp.as_ptr(),
            bp.as_ptr(),
            ptr::addr_of_mut!(info),
            ptr::addr_of_mut!(plies),
        )
    };

    let plies = plies as i32;

    match gaviota_sys::TB_return_values(info) {
        gaviota_sys::TB_return_values::tb_DRAW if result != 0 => Some(0),
        gaviota_sys::TB_return_values::tb_WMATE if result != 0 => {
            Some(pos.turn().fold_wb(plies, -plies))
        }
        gaviota_sys::TB_return_values::tb_BMATE if result != 0 => {
            Some(pos.turn().fold_wb(-plies, plies))
        }
        gaviota_sys::TB_return_values::tb_FORBID => None,
        _ => {
            warn!("gaviota probe failed with result {result} and info {info}");
            None
        }
    }
}

#[derive(Debug)]
struct Tablebases {
    standard: SyzygyTablebase<Chess>,
    atomic: SyzygyTablebase<Atomic>,
    antichess: SyzygyTablebase<Antichess>,
}

impl Tablebases {
    fn probe_dtz(&self, pos: &VariantPosition) -> Result<MaybeRounded<Dtz>, SyzygyError> {
        match *pos {
            VariantPosition::Chess(ref pos) => self.standard.probe_dtz(pos),
            VariantPosition::Atomic(ref pos) => self.atomic.probe_dtz(pos),
            VariantPosition::Antichess(ref pos) => self.antichess.probe_dtz(pos),
            _ => unimplemented!("variant not supported"),
        }
    }

    fn best_move(
        &self,
        pos: &VariantPosition,
    ) -> Result<Option<(Move, MaybeRounded<Dtz>)>, SyzygyError> {
        match *pos {
            VariantPosition::Chess(ref pos) => self.standard.best_move(pos),
            VariantPosition::Atomic(ref pos) => self.atomic.best_move(pos),
            VariantPosition::Antichess(ref pos) => self.antichess.best_move(pos),
            _ => unimplemented!("variant not supported"),
        }
    }

    fn position_info(&self, pos: &VariantPosition) -> Result<PositionInfo, SyzygyError> {
        let (variant_win, variant_loss) = match pos.variant_outcome() {
            Some(Outcome::Decisive { winner }) => (winner == pos.turn(), winner != pos.turn()),
            _ => (false, false),
        };

        let dtz = match self.probe_dtz(pos) {
            Err(
                SyzygyError::Castling
                | SyzygyError::TooManyPieces
                | SyzygyError::MissingTable { .. },
            ) => None, // user error
            Err(err) => return Err(err), // server error
            Ok(res) => Some(res),
        };

        Ok(PositionInfo {
            checkmate: pos.is_checkmate(),
            stalemate: pos.is_stalemate(),
            variant_win,
            variant_loss,
            insufficient_material: pos.is_insufficient_material(),
            maybe_rounded_dtz: dtz.map(MaybeRounded::ignore_rounding),
            precise_dtz: dtz.and_then(MaybeRounded::precise),
            dtz,
            dtm: unsafe { probe_dtm(pos) },
            halfmoves: pos.halfmoves(),
        })
    }

    fn probe(&self, pos: &VariantPosition) -> Result<TablebaseResponse, SyzygyError> {
        let halfmoves = pos.halfmoves();

        let mut move_info = pos
            .legal_moves()
            .iter()
            .map(|m| {
                let mut after = pos.clone();
                after.play_unchecked(m);

                let after_info = self.position_info(&after)?;

                Ok(MoveInfo {
                    uci: m.to_uci(pos.castles().mode()),
                    san: SanPlus::from_move(pos.clone(), m),
                    capture: m.capture(),
                    promotion: m.promotion(),
                    zeroing: m.is_zeroing(),
                    category: after_info.category(halfmoves),
                    pos: after_info,
                })
            })
            .collect::<Result<ArrayVec<_, 256>, SyzygyError>>()?;

        move_info.sort_by_key(|m: &MoveInfo| {
            (
                PessimisticUnknown(m.category),
                (
                    Reverse(m.pos.checkmate),
                    Reverse(m.pos.variant_loss),
                    m.pos.variant_win,
                ),
                (
                    Reverse(m.pos.stalemate),
                    Reverse(m.pos.insufficient_material),
                ),
                if m.pos
                    .dtz
                    .unwrap_or(MaybeRounded::Precise(Dtz(0)))
                    .is_negative()
                {
                    Reverse(m.pos.dtm)
                } else {
                    Reverse(None)
                },
                if m.pos
                    .dtz
                    .unwrap_or(MaybeRounded::Precise(Dtz(0)))
                    .is_positive()
                {
                    m.pos.dtm.map(Reverse)
                } else {
                    None
                },
                m.zeroing
                    ^ !m.pos
                        .dtz
                        .unwrap_or(MaybeRounded::Precise(Dtz(0)))
                        .is_positive(),
                m.capture.is_some()
                    ^ !m.pos
                        .dtz
                        .unwrap_or(MaybeRounded::Precise(Dtz(0)))
                        .is_positive(),
                m.pos.maybe_rounded_dtz.map(Reverse),
                (Reverse(m.capture), Reverse(m.promotion)),
            )
        });

        let pos_info = self.position_info(pos)?;
        let category = pos_info.category(halfmoves.saturating_sub(1));

        // Use category of previous position to infer maybe-win / maybe-loss,
        // if possible.
        for (prev, maybe, correct) in [
            (Category::Win, Category::MaybeLoss, Category::Loss),
            (
                Category::CursedWin,
                Category::MaybeLoss,
                Category::BlessedLoss,
            ),
            (
                Category::BlessedLoss,
                Category::MaybeWin,
                Category::CursedWin,
            ),
            (Category::Loss, Category::MaybeWin, Category::Win),
        ] {
            if category == prev {
                for m in &mut move_info {
                    if m.category != maybe {
                        break;
                    }
                    m.category = correct;
                }
            }
        }

        Ok(TablebaseResponse {
            pos: pos_info,
            category: move_info.first().map_or(category, |m| -m.category),
            moves: move_info,
        })
    }

    fn mainline(&self, mut pos: VariantPosition) -> Result<MainlineResponse, SyzygyError> {
        let dtz = self.probe_dtz(&pos)?;
        let mut mainline = Vec::new();

        if !dtz.is_zero() {
            while pos.halfmoves() < 100 {
                if let Some((m, dtz)) = self.best_move(&pos)? {
                    mainline.push(MainlineStep {
                        uci: m.to_uci(pos.castles().mode()),
                        dtz: dtz.ignore_rounding(),
                        precise_dtz: dtz.precise(),
                        san: SanPlus::from_move_and_play_unchecked(&mut pos, &m),
                    });
                } else {
                    break;
                }
            }
        }

        Ok(MainlineResponse {
            dtz: dtz.ignore_rounding(),
            precise_dtz: dtz.precise(),
            mainline,
            winner: pos
                .outcome()
                .and_then(|o| o.winner())
                .map(|winner| winner.char()),
        })
    }
}

#[derive(Debug, Clone)]
enum TablebaseError {
    Fen(ParseFenError),
    Position(Arc<PositionError<VariantPosition>>),
    Syzygy(Arc<SyzygyError>),
}

impl From<ParseFenError> for TablebaseError {
    fn from(v: ParseFenError) -> TablebaseError {
        TablebaseError::Fen(v)
    }
}

impl From<Arc<PositionError<VariantPosition>>> for TablebaseError {
    fn from(v: Arc<PositionError<VariantPosition>>) -> TablebaseError {
        TablebaseError::Position(v)
    }
}

impl From<SyzygyError> for TablebaseError {
    fn from(v: SyzygyError) -> TablebaseError {
        TablebaseError::Syzygy(Arc::new(v))
    }
}

impl IntoResponse for TablebaseError {
    fn into_response(self) -> Response {
        (match self {
            TablebaseError::Fen(err) => (StatusCode::BAD_REQUEST, err.to_string()),
            TablebaseError::Position(err) => (StatusCode::BAD_REQUEST, err.to_string()),
            TablebaseError::Syzygy(err) => match *err {
                SyzygyError::Castling | SyzygyError::TooManyPieces => {
                    (StatusCode::NOT_FOUND, err.to_string())
                }
                SyzygyError::MissingTable { .. } => {
                    warn!("{err}");
                    (StatusCode::NOT_FOUND, err.to_string())
                }
                SyzygyError::ProbeFailed { .. } => {
                    error!("{err}");
                    (StatusCode::INTERNAL_SERVER_ERROR, err.to_string())
                }
            },
        })
        .into_response()
    }
}

#[serde_as]
#[derive(Deserialize, Debug, Clone, Eq, PartialEq, Hash)]
struct TablebaseQuery {
    #[serde_as(as = "DisplayFromStr")]
    fen: Fen,
}

fn try_probe(
    tbs: &Tablebases,
    variant: TablebaseVariant,
    query: TablebaseQuery,
) -> Result<TablebaseResponse, TablebaseError> {
    match variant {
        TablebaseVariant::Standard => info!("standard: {}", &query.fen),
        TablebaseVariant::Atomic => info!("atomic: {}", &query.fen),
        TablebaseVariant::Antichess => info!("antichess: {}", &query.fen),
    }

    Ok(tbs.probe(&variant.position(query.fen)?)?)
}

fn try_mainline(
    tbs: &Tablebases,
    variant: TablebaseVariant,
    query: TablebaseQuery,
) -> Result<MainlineResponse, TablebaseError> {
    match variant {
        TablebaseVariant::Standard => info!("standard mainline: {}", &query.fen),
        TablebaseVariant::Atomic => info!("atomic mainline: {}", &query.fen),
        TablebaseVariant::Antichess => info!("antichess mainline: {}", &query.fen),
    }

    Ok(tbs.mainline(variant.position(query.fen)?)?)
}

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

    /// Maximum number of cached responses.
    #[arg(long, default_value = "20000")]
    cache: u64,

    /// Listen on this socket address.
    #[arg(long, default_value = "127.0.0.1:9000")]
    bind: SocketAddr,
}

type TablebaseCache = Cache<(TablebaseVariant, TablebaseQuery), TablebaseResponse>;

struct AppState {
    tbs: Tablebases,
    cache: TablebaseCache,
    cache_miss: AtomicU64,
    semaphore: Semaphore,
    deploy_event_sent: AtomicBool,
}

fn main() {
    let subscriber = tracing_timing::Builder::default()
        .build(|| tracing_timing::Histogram::new(3).expect("histogram"));
    let dispatcher = tracing::Dispatch::new(subscriber);
    tracing::dispatcher::set_global_default(dispatcher).expect("set tracing default dispatcher");
    tokio::runtime::Builder::new_multi_thread()
        .enable_all()
        .max_blocking_threads(128)
        .build()
        .expect("tokio runtime")
        .block_on(serve());
}

async fn serve() {
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

    // Initialize Gaviota tablebase.
    if !opt.gaviota.is_empty() {
        unsafe {
            assert!(gaviota_sys::tbcache_init(1014 * 1024, 50) != 0);
            let mut paths = gaviota_sys::tbpaths_init();
            assert!(!paths.is_null());
            for path in opt.gaviota {
                let path = CString::new(path.as_os_str().to_str().unwrap()).unwrap();
                paths = gaviota_sys::tbpaths_add(paths, path.as_ptr());
                assert!(!paths.is_null());
                drop(path);
            }
            assert!(!gaviota_sys::tb_init(
                1,
                gaviota_sys::TB_compression_scheme::tb_CP4 as c_int,
                paths
            )
            .is_null());
        }
    }

    // Initialize Syzygy tablebases.
    let state: &'static AppState = Box::leak(Box::new(AppState {
        tbs: {
            let mut standard = unsafe { SyzygyTablebase::<Chess>::with_mmap_filesystem() };
            let mut atomic = unsafe { SyzygyTablebase::<Atomic>::with_mmap_filesystem() };
            let mut antichess = unsafe { SyzygyTablebase::<Antichess>::with_mmap_filesystem() };

            for path in opt.standard {
                let n = standard
                    .add_directory(&path)
                    .expect("open standard directory");
                info!("added {} standard tables from {}", n, path.display());
            }
            for path in opt.atomic {
                let n = atomic.add_directory(&path).expect("open atomic directory");
                info!("added {} atomic tables from {}", n, path.display());
            }
            for path in opt.antichess {
                let n = antichess
                    .add_directory(&path)
                    .expect("open antichess directory");
                info!("added {} antichess tables from {}", n, path.display());
            }

            Tablebases {
                standard,
                atomic,
                antichess,
            }
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

async fn handle_monitor(State(app): State<&'static AppState>) -> String {
    if app
        .deploy_event_sent
        .fetch_or(true, atomic::Ordering::Relaxed)
    {
        let cache = app.cache.entry_count();
        let cache_miss = app.cache_miss.load(atomic::Ordering::Relaxed);
        let mut metrics = vec![
            format!("cache={cache}u"),
            format!("cache_miss={cache_miss}u"),
        ];
        tracing::dispatcher::get_default(|dispatcher: &tracing::Dispatch| {
            let subscriber = dispatcher
                .downcast_ref::<TimingSubscriber>()
                .expect("timing subscriber");
            subscriber.force_synchronize();
            subscriber.with_histograms(|hs| {
                for (span_group, hs) in hs {
                    let span_group = span_group.replace(' ', "_");
                    if span_group == "dtz_table"
                        || span_group == "wdl_table"
                        || span_group == "request"
                    {
                        for (event_group, h) in hs {
                            let event_group = event_group.replace(' ', "_");
                            metrics.extend([
                                format!("{span_group}_{event_group}_count={}u", h.len()),
                                format!(
                                    "{span_group}_{event_group}_p50={}u",
                                    h.value_at_quantile(0.50)
                                ),
                                format!(
                                    "{span_group}_{event_group}_p90={}u",
                                    h.value_at_quantile(0.90)
                                ),
                                format!(
                                    "{span_group}_{event_group}_p99={}u",
                                    h.value_at_quantile(0.99)
                                ),
                                format!(
                                    "{span_group}_{event_group}_p999={}u",
                                    h.value_at_quantile(0.999)
                                ),
                                format!("{span_group}_{event_group}_max={}u", h.max()),
                            ]);
                            h.reset();
                        }
                    }
                }
            });
        });
        format!("tablebase {}", metrics.join(","))
    } else {
        format!(
            "event,program=lila-tablebase commit={:?},text={:?}",
            env!("VERGEN_GIT_SHA"),
            env!("VERGEN_GIT_COMMIT_MESSAGE")
        )
    }
}

async fn handle_probe(
    State(app): State<&'static AppState>,
    Path(variant): Path<TablebaseVariant>,
    Query(query): Query<TablebaseQuery>,
) -> Result<Json<TablebaseResponse>, TablebaseError> {
    app.cache
        .try_get_with((variant, query.clone()), async move {
            app.cache_miss.fetch_add(1, atomic::Ordering::Relaxed);
            let _permit = app.semaphore.acquire().await.expect("semaphore not closed");
            task::spawn_blocking(move || try_probe(&app.tbs, variant, query))
                .await
                .expect("blocking probe")
        })
        .await
        .map(Json)
        .map_err(Arc::unwrap_or_clone)
}

async fn handle_mainline(
    State(app): State<&'static AppState>,
    Path(variant): Path<TablebaseVariant>,
    Query(query): Query<TablebaseQuery>,
) -> Result<Json<MainlineResponse>, TablebaseError> {
    let _permit = app.semaphore.acquire().await.expect("semaphore not closed");
    task::spawn_blocking(move || try_mainline(&app.tbs, variant, query))
        .await
        .expect("blocking mainline")
        .map(Json)
}
