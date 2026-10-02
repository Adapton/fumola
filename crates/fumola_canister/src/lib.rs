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
//! Stage 1 stores Hazel's text as it is, each value in a cell of an Adapton
//! DCG (see `store`): a save is a Fumola put, a read forces the cell. `eval`
//! runs a Fumola program in the same state, so it can read those cells.
//! Storing the state as Fumola values, rather than text, is stage 2.

mod store;
use store::Store;

use candid::{CandidType, Deserialize};
use serde_bytes::ByteBuf;
use std::cell::RefCell;

thread_local! {
    static KV: RefCell<Store> = RefCell::new(Store::new());
    static LOG: RefCell<Vec<(String, String)>> = RefCell::new(Vec::new());
}

/// One write in a batch: Hazel's `kv_batch` groups its writes, and sends
/// them as one request.
#[derive(CandidType, Deserialize, serde::Serialize, Clone, Debug)]
#[serde(tag = "op", rename_all = "lowercase")]
pub enum Write {
    Put { key: String, value: String },
    Remove { key: String },
}

fn apply(writes: Vec<Write>) -> usize {
    KV.with(|kv| {
        let mut kv = kv.borrow_mut();
        for w in &writes {
            match w {
                Write::Put { key, value } => {
                    if let Err(e) = kv.put(key, value) {
                        ic_cdk::println!("put {} failed: {}", key, e);
                    }
                }
                Write::Remove { key } => kv.remove(key),
            }
        }
    });
    writes.len()
}

// ---- Candid interface ------------------------------------------------------

#[ic_cdk::query]
fn kv_all() -> Vec<(String, String)> {
    KV.with(|kv| kv.borrow_mut().all())
}

#[ic_cdk::query]
fn kv_get(key: String) -> Option<String> {
    KV.with(|kv| kv.borrow_mut().get(&key))
}

#[ic_cdk::update]
fn kv_apply(writes: Vec<Write>) -> u64 {
    apply(writes) as u64
}

#[ic_cdk::update]
fn kv_clear() {
    KV.with(|kv| kv.borrow_mut().clear());
}

/// How many cells the DCG holds, counting each put's version.
#[ic_cdk::query]
fn cells() -> String {
    KV.with(|kv| kv.borrow_mut().cell_count())
}

#[ic_cdk::update]
fn log_add(key: String, value: String) {
    LOG.with(|log| log.borrow_mut().push((key, value)));
}

#[ic_cdk::query]
fn log_all() -> Vec<String> {
    LOG.with(|log| log.borrow().iter().map(|(_, v)| v.clone()).collect())
}

#[ic_cdk::update]
fn log_clear() {
    LOG.with(|log| log.borrow_mut().clear());
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

#[ic_cdk::pre_upgrade]
fn pre_upgrade() {
    let kv = KV.with(|kv| kv.borrow_mut().snapshot());
    let log = LOG.with(|log| log.borrow().clone());
    ic_cdk::storage::stable_save((kv, log)).expect("saving state to stable memory");
}

#[ic_cdk::post_upgrade]
fn post_upgrade() {
    let (kv, log): (Vec<(String, String)>, Vec<(String, String)>) =
        ic_cdk::storage::stable_restore().expect("restoring state from stable memory");
    KV.with(|k| k.borrow_mut().restore(kv));
    LOG.with(|l| *l.borrow_mut() = log);
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

#[ic_cdk::query]
fn http_request(req: HttpRequest) -> HttpResponse {
    let path = path_of(&req.url);
    match (req.method.as_str(), path) {
        ("OPTIONS", _) => response(204, "text/plain", String::new()),
        ("GET", "/kv") => {
            let all: serde_json::Map<String, serde_json::Value> = KV.with(|kv| {
                kv.borrow_mut()
                    .all()
                    .into_iter()
                    .map(|(k, v)| (k, serde_json::Value::String(v)))
                    .collect()
            });
            json(serde_json::Value::Object(all).to_string())
        }
        ("GET", "/log") => json(serde_json::to_string(&log_all()).unwrap_or_default()),
        ("GET", "/") | ("GET", "/health") => response(
            200,
            "text/plain",
            format!(
                "fumola_canister: {} keys in {} DCG cells, {} log entries",
                KV.with(|kv| kv.borrow().keys().len()),
                cells(),
                LOG.with(|log| log.borrow().len())
            ),
        ),
        ("POST", _) => HttpResponse {
            upgrade: Some(true),
            ..response(200, "text/plain", String::new())
        },
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
    match (req.method.as_str(), path.as_str()) {
        ("POST", "/kv") => match serde_json::from_str::<Vec<Write>>(&body) {
            Ok(writes) => json(format!("{{\"applied\":{}}}", apply(writes))),
            Err(e) => response(400, "text/plain", format!("bad writes: {}", e)),
        },
        ("POST", "/kv/clear") => {
            kv_clear();
            json("{\"cleared\":true}".to_string())
        }
        ("POST", "/log") => match serde_json::from_str::<LogEntry>(&body) {
            Ok(entry) => {
                log_add(entry.key, entry.value);
                json("{\"logged\":true}".to_string())
            }
            Err(e) => response(400, "text/plain", format!("bad log entry: {}", e)),
        },
        ("POST", "/log/clear") => {
            log_clear();
            json("{\"cleared\":true}".to_string())
        }
        ("POST", "/eval") => response(200, "text/plain", eval_text(&body)),
        _ => not_found(&path),
    }
}

ic_cdk::export_candid!();
