import gleam/dynamic.{type Dynamic}
import gleam/javascript/array.{type Array}

// TODO: docs
// TODO: labels
pub type Promise(value)

@external(javascript, "../../ffi.mjs", "newPromise")
pub fn new(a: fn(fn(value) -> Nil) -> Nil) -> Promise(value)

@external(javascript, "../../ffi.mjs", "resolve")
pub fn resolve(a: value) -> Promise(value)

@external(javascript, "../../ffi.mjs", "rescue")
pub fn rescue(a: Promise(value), b: fn(Dynamic) -> value) -> Promise(value)

@external(javascript, "../../ffi.mjs", "then")
pub fn await(a: Promise(a), b: fn(a) -> Promise(b)) -> Promise(b)

@external(javascript, "../../ffi.mjs", "map_promise")
pub fn map(a: Promise(a), b: fn(a) -> b) -> Promise(b)

pub fn tap(promise: Promise(a), callback: fn(a) -> b) -> Promise(a) {
  promise
  |> map(fn(a) {
    callback(a)
    a
  })
}

pub fn map_try(
  promise: Promise(Result(a, e)),
  callback: fn(a) -> Result(b, e),
) -> Promise(Result(b, e)) {
  promise
  |> map(fn(result) {
    case result {
      Ok(a) -> callback(a)
      Error(e) -> Error(e)
    }
  })
}

pub fn try_await(
  promise: Promise(Result(a, e)),
  callback: fn(a) -> Promise(Result(b, e)),
) -> Promise(Result(b, e)) {
  promise
  |> await(fn(result) {
    case result {
      Ok(a) -> callback(a)
      Error(e) -> resolve(Error(e))
    }
  })
}

@external(javascript, "../../ffi.mjs", "all_promises")
pub fn await2(a: Promise(a), b: Promise(b)) -> Promise(#(a, b))

@external(javascript, "../../ffi.mjs", "all_promises")
pub fn await3(
  a: Promise(a),
  b: Promise(b),
  c: Promise(c),
) -> Promise(#(a, b, c))

@external(javascript, "../../ffi.mjs", "all_promises")
pub fn await4(
  a: Promise(a),
  b: Promise(b),
  c: Promise(c),
  d: Promise(d),
) -> Promise(#(a, b, c, d))

@external(javascript, "../../ffi.mjs", "all_promises")
pub fn await5(
  a: Promise(a),
  b: Promise(b),
  c: Promise(c),
  d: Promise(d),
  e: Promise(e),
) -> Promise(#(a, b, c, d, e))

@external(javascript, "../../ffi.mjs", "all_promises")
pub fn await6(
  a: Promise(a),
  b: Promise(b),
  c: Promise(c),
  d: Promise(d),
  e: Promise(e),
  f: Promise(f),
) -> Promise(#(a, b, c, d, e, f))

@external(javascript, "../../ffi.mjs", "all_promises")
pub fn await_array(a: Array(Promise(a))) -> Promise(Array(a))

pub fn await_list(xs: List(Promise(a))) -> Promise(List(a)) {
  xs
  |> do_await_list
  |> map(array.to_list)
}

@external(javascript, "../../ffi.mjs", "all_promises")
fn do_await_list(a: List(Promise(a))) -> Promise(Array(a))
