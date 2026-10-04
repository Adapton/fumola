//! A canister that holds Hazel's state, with the Fumola interpreter linked in.
//!
//! Hazel persists everything through two tables of text (its `HazelDB`
//! module): `kv`, a key-value table of app state, and `log`, an
//! append-only action log. This canister keeps both, answering the same
//! operations two ways:
//!
//! - as Candid methods, for `icp canister call`;
//! - over HTTP, through the replica's gateway, so Hazel's front end can
//!   use plain `fetch` and needs no IC agent library.
//!
//! Each value lives in a cell of an Adapton DCG (see `store`): a save is a
//! Fumola put, a read forces the cell. Stage 2: the cell holds the value as
//! a Fumola value, not its text -- Hazel saves S-expressions, stored as
//! `#atom` / `#list` values (see `sexp`) and printed back on a read. `eval`
//! runs a Fumola program in the same state, so it can walk those values.

mod schema;
mod sexp;
mod store;
use store::{Store, DEFAULT_SPACE};
use std::collections::BTreeMap;

use candid::{CandidType, Deserialize};
use serde_bytes::ByteBuf;
use std::cell::RefCell;

thread_local! {
    static KV: RefCell<Store> = RefCell::new(Store::new());
    /// Each space's action log, oldest first.
    static LOG: RefCell<BTreeMap<String, Vec<(String, String)>>> = RefCell::new(BTreeMap::new());
}

/// One write in a batch: Hazel's `kv_batch` groups its writes, and sends
/// them as one request.
#[derive(CandidType, Deserialize, serde::Serialize, Clone, Debug)]
#[serde(tag = "op", rename_all = "lowercase")]
pub enum Write {
    Put { key: String, value: String },
    Remove { key: String },
}

fn apply(space: &str, writes: Vec<Write>) -> usize {
    KV.with(|kv| {
        let mut kv = kv.borrow_mut();
        for w in &writes {
            match w {
                Write::Put { key, value } => {
                    if let Err(e) = kv.put(space, key, value) {
                        ic_cdk::println!("put {}/{} failed: {}", space, key, e);
                    }
                }
                Write::Remove { key } => kv.remove(space, key),
            }
        }
    });
    writes.len()
}

fn log_of(space: &str) -> Vec<String> {
    LOG.with(|log| {
        log.borrow()
            .get(space)
            .map(|es| es.iter().map(|(_, v)| v.clone()).collect())
            .unwrap_or_default()
    })
}

fn log_push(space: &str, key: String, value: String) {
    LOG.with(|log| {
        log.borrow_mut()
            .entry(space.to_string())
            .or_default()
            .push((key, value))
    });
}

fn log_clear_space(space: &str) {
    LOG.with(|log| {
        log.borrow_mut().remove(space);
    });
}

/// A space's name: what a front end opts in under. Letters, digits, `-`
/// and `_`, up to 64 of them, so it reads cleanly in a path.
fn valid_space(s: &str) -> bool {
    !s.is_empty()
        && s.len() <= 64
        && s.chars().all(|c| c.is_ascii_alphanumeric() || c == '-' || c == '_')
}

// ---- Candid interface ------------------------------------------------------

// The methods without a space act on Hazel's own, DEFAULT_SPACE.

#[ic_cdk::query]
fn kv_all() -> Vec<(String, String)> {
    KV.with(|kv| kv.borrow_mut().all(DEFAULT_SPACE))
}

#[ic_cdk::query]
fn kv_get(key: String) -> Option<String> {
    KV.with(|kv| kv.borrow_mut().get(DEFAULT_SPACE, &key))
}

#[ic_cdk::update]
fn kv_apply(writes: Vec<Write>) -> u64 {
    apply(DEFAULT_SPACE, writes) as u64
}

#[ic_cdk::update]
fn kv_clear() {
    KV.with(|kv| kv.borrow_mut().clear(DEFAULT_SPACE));
}

/// Every space holding keys, with how many.
#[ic_cdk::query]
fn spaces() -> Vec<(String, u64)> {
    KV.with(|kv| {
        kv.borrow()
            .spaces()
            .into_iter()
            .map(|(s, n)| (s, n as u64))
            .collect()
    })
}

/// How many cells the DCG holds, counting each put's version.
#[ic_cdk::query]
fn cells() -> String {
    KV.with(|kv| kv.borrow_mut().cell_count())
}

#[ic_cdk::update]
fn log_add(key: String, value: String) {
    log_push(DEFAULT_SPACE, key, value);
}

#[ic_cdk::query]
fn log_all() -> Vec<String> {
    log_of(DEFAULT_SPACE)
}

#[ic_cdk::update]
fn log_clear() {
    log_clear_space(DEFAULT_SPACE);
}

/// Run a Fumola program in the store's state and answer its value, printed
/// on one line, or the error. An update: what it defines or puts persists.
#[ic_cdk::update]
fn eval(program: String) -> String {
    eval_text(&program)
}

fn eval_text(program: &str) -> String {
    KV.with(|kv| kv.borrow_mut().eval(program))
}

// ---- Upgrades --------------------------------------------------------------

/// The upgrade snapshot: a version, then each space's keys and log.
type Snapshot = (
    u32,
    Vec<(String, Vec<(String, String)>)>,
    Vec<(String, Vec<(String, String)>)>,
);
/// Before spaces: one key table and one log, now DEFAULT_SPACE's.
type SnapshotV1 = (Vec<(String, String)>, Vec<(String, String)>);

#[ic_cdk::pre_upgrade]
fn pre_upgrade() {
    let kv = KV.with(|kv| kv.borrow_mut().snapshot());
    let log: Vec<(String, Vec<(String, String)>)> =
        LOG.with(|log| log.borrow().clone().into_iter().collect());
    let snap: Snapshot = (2, kv, log);
    ic_cdk::storage::stable_save(snap).expect("saving state to stable memory");
}

#[ic_cdk::post_upgrade]
fn post_upgrade() {
    let (kv, log) = match ic_cdk::storage::stable_restore::<Snapshot>() {
        Ok((_, kv, log)) => (kv, log),
        Err(_) => {
            let (kv, log): SnapshotV1 = ic_cdk::storage::stable_restore()
                .expect("restoring state from stable memory");
            (
                vec![(DEFAULT_SPACE.to_string(), kv)],
                vec![(DEFAULT_SPACE.to_string(), log)],
            )
        }
    };
    KV.with(|k| k.borrow_mut().restore(kv));
    LOG.with(|l| *l.borrow_mut() = log.into_iter().collect());
}

// ---- HTTP ------------------------------------------------------------------
//
//   GET  /kv        every key and value, as a JSON object
//   POST /kv        a JSON list of writes, {"op":"put","key","value"} or
//                   {"op":"remove","key"}, applied in order
//   POST /kv/clear  empty the table
//   GET  /log       the log's values, oldest first, as a JSON list
//   POST /log       append {"key","value"}
//   POST /eval      run the body as a Fumola program
//   GET  /spaces    every space holding keys, with how many, as JSON
//
// Each path but /eval and /spaces is in a SPACE: `/s/<space>/kv`,
// `/s/<space>/log` and so on are that space's, and the paths without
// `/s/` are Hazel's own space, DEFAULT_SPACE, as before spaces. A front
// end opts in to a space by naming it (window.hazelBackend in Hazel).
//
// A GET is a query. A POST is answered by the query with `upgrade`, which
// has the gateway call `http_request_update`, where it is applied. Every
// response allows any origin, since Hazel is served from another canister.
// The responses are not certified, so the front end uses the gateway's
// `raw` address for this canister.

#[derive(CandidType, Deserialize)]
pub struct HttpRequest {
    method: String,
    url: String,
    headers: Vec<(String, String)>,
    body: ByteBuf,
    certificate_version: Option<u16>,
}

#[derive(CandidType, Deserialize)]
pub struct HttpUpdateRequest {
    method: String,
    url: String,
    headers: Vec<(String, String)>,
    body: ByteBuf,
}

#[derive(CandidType, Deserialize)]
pub struct HttpResponse {
    status_code: u16,
    headers: Vec<(String, String)>,
    body: ByteBuf,
    upgrade: Option<bool>,
}

fn path_of(url: &str) -> &str {
    url.split('?').next().unwrap_or(url)
}

fn response(status_code: u16, content_type: &str, body: String) -> HttpResponse {
    HttpResponse {
        status_code,
        headers: vec![
            ("Content-Type".to_string(), content_type.to_string()),
            ("Access-Control-Allow-Origin".to_string(), "*".to_string()),
            (
                "Access-Control-Allow-Methods".to_string(),
                "GET, POST, OPTIONS".to_string(),
            ),
            (
                "Access-Control-Allow-Headers".to_string(),
                "Content-Type".to_string(),
            ),
        ],
        body: ByteBuf::from(body.into_bytes()),
        upgrade: None,
    }
}

fn json(body: String) -> HttpResponse {
    response(200, "application/json", body)
}

fn not_found(path: &str) -> HttpResponse {
    response(404, "text/plain", format!("no such path: {}", path))
}

/// A request's space and the path within it: `/s/<space>/kv` is `kv` in
/// that space, and a path without `/s/` is in DEFAULT_SPACE, as before
/// spaces. None for a space name that is not a valid one.
fn route(path: &str) -> Option<(String, String)> {
    match path.strip_prefix("/s/") {
        Some(rest) => {
            let (space, within) = match rest.split_once('/') {
                Some((s, w)) => (s, format!("/{}", w)),
                None => (rest, "/".to_string()),
            };
            if valid_space(space) {
                Some((space.to_string(), within))
            } else {
                None
            }
        }
        None => Some((DEFAULT_SPACE.to_string(), path.to_string())),
    }
}

fn bad_space(path: &str) -> HttpResponse {
    response(
        400,
        "text/plain",
        format!(
            "no such space in {}: a space is letters, digits, - and _, up to 64",
            path
        ),
    )
}

#[ic_cdk::query]
fn http_request(req: HttpRequest) -> HttpResponse {
    let path = path_of(&req.url);
    if req.method == "OPTIONS" {
        return response(204, "text/plain", String::new());
    }
    if req.method == "POST" {
        return HttpResponse {
            upgrade: Some(true),
            ..response(200, "text/plain", String::new())
        };
    }
    if req.method == "GET" && path == "/spaces" {
        let all: serde_json::Map<String, serde_json::Value> = spaces()
            .into_iter()
            .map(|(s, n)| (s, serde_json::Value::from(n)))
            .collect();
        return json(serde_json::Value::Object(all).to_string());
    }
    let (space, within) = match route(path) {
        Some(r) => r,
        None => return bad_space(path),
    };
    match (req.method.as_str(), within.as_str()) {
        ("GET", "/kv") => {
            let all: serde_json::Map<String, serde_json::Value> = KV.with(|kv| {
                kv.borrow_mut()
                    .all(&space)
                    .into_iter()
                    .map(|(k, v)| (k, serde_json::Value::String(v)))
                    .collect()
            });
            json(serde_json::Value::Object(all).to_string())
        }
        ("GET", "/log") => json(serde_json::to_string(&log_of(&space)).unwrap_or_default()),
        ("GET", "/index") => {
            let index: serde_json::Map<String, serde_json::Value> = KV.with(|kv| {
                kv.borrow()
                    .index(&space)
                    .into_iter()
                    .map(|(k, n)| (k, serde_json::Value::from(n)))
                    .collect()
            });
            json(serde_json::Value::Object(index).to_string())
        }
        ("GET", "/") | ("GET", "/health") => {
            let shapes = KV.with(|kv| kv.borrow_mut().shapes());
            response(
                200,
                "text/plain",
                format!(
                    "fumola_canister: {} keys in {} spaces, {} DCG cells ({} as Hazel's types, {} as sexp values, {} as text), {} log entries in this space ({})",
                    KV.with(|kv| kv.borrow().key_count()),
                    spaces().len(),
                    cells(),
                    shapes.0,
                    shapes.1,
                    shapes.2,
                    log_of(&space).len(),
                    space
                ),
            )
        }
        _ => not_found(path),
    }
}

#[derive(Deserialize)]
struct LogEntry {
    key: String,
    value: String,
}

#[ic_cdk::update]
fn http_request_update(req: HttpUpdateRequest) -> HttpResponse {
    let path = path_of(&req.url).to_string();
    let body = String::from_utf8_lossy(&req.body).to_string();
    if req.method == "POST" && path == "/eval" {
        return response(200, "text/plain", eval_text(&body));
    }
    let (space, within) = match route(&path) {
        Some(r) => r,
        None => return bad_space(&path),
    };
    match (req.method.as_str(), within.as_str()) {
        ("POST", "/kv") => match serde_json::from_str::<Vec<Write>>(&body) {
            Ok(writes) => json(format!("{{\"applied\":{}}}", apply(&space, writes))),
            Err(e) => response(400, "text/plain", format!("bad writes: {}", e)),
        },
        ("POST", "/kv/clear") => {
            KV.with(|kv| kv.borrow_mut().clear(&space));
            json("{\"cleared\":true}".to_string())
        }
        ("POST", "/log") => match serde_json::from_str::<LogEntry>(&body) {
            Ok(entry) => {
                log_push(&space, entry.key, entry.value);
                json("{\"logged\":true}".to_string())
            }
            Err(e) => response(400, "text/plain", format!("bad log entry: {}", e)),
        },
        ("POST", "/log/clear") => {
            log_clear_space(&space);
            json("{\"cleared\":true}".to_string())
        }
        _ => not_found(&path),
    }
}

ic_cdk::export_candid!();
