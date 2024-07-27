//// BEAM only.

import gleam/erlang/process.{type Subject}
import gleam/otp/actor
import gleam/result
import sketch/error
import sketch/internals/cache/state
import sketch/internals/class
import sketch/internals/style
import sketch/options.{type Options}

/// Manages the styles. Can be instanciated with [`create_cache`](#create_cache).
pub opaque type Cache {
  Cache(subject: Subject(state.Request))
}

@external(erlang, "sketch_ffi", "save_current_cache")
fn save_current_cache(cache: Cache) -> Nil

@external(erlang, "sketch_ffi", "get_current_cache")
fn get_current_cache() -> Result(Cache, Nil)

@external(erlang, "sketch_ffi", "stacktrace")
fn stacktrace() -> String

pub fn create_cache(_options: Options) -> Result(Cache, error.SketchError) {
  let assert Ok(subject) = actor.start(state.init(), state.loop)
  Ok(Cache(subject))
}

pub fn prepare(cache: Cache) -> Nil {
  let Cache(subject) = cache
  save_current_cache(cache)
  process.send(subject, state.Prepare)
}

pub fn render(cache: Cache) -> Result(String, Nil) {
  let Cache(subject) = cache
  process.try_call(subject, state.Diff, 1000)
  |> result.nil_error()
}

pub fn compile_class(styles: List(style.Style)) -> class.Class {
  let cache = get_current_cache()
  let st = stacktrace()
  cache
  |> result.try(fn(cache) {
    let Cache(subject) = cache
    let persist = fn(s) { state.Persist(st, styles, s) }
    process.try_call(subject, persist, within: 100)
    |> result.nil_error()
  })
  |> result.unwrap(class.no_class())
}

pub fn compile_style(styles: List(style.Style), id: String) -> class.Class {
  let cache = get_current_cache()
  cache
  |> result.try(fn(cache) {
    let Cache(subject) = cache
    let persist = fn(s) { state.Persist(id, styles, s) }
    process.try_call(subject, persist, within: 100)
    |> result.nil_error()
  })
  |> result.unwrap(class.no_class())
}

pub fn memo(class: class.Class) -> class.Class {
  case get_current_cache() {
    Ok(Cache(subject)) -> process.send(subject, state.Memoize(class))
    _ -> Nil
  }
  class
}
