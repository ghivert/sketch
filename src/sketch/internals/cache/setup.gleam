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
fn get_current_cache() -> Cache

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

pub fn render(cache: Cache) -> String {
  let Cache(subject) = cache
  let res = process.try_call(subject, state.Diff, 1000)
  result.unwrap(res, "")
}

pub fn compile_class(styles: List(style.Style)) -> class.Class {
  let Cache(subject) = get_current_cache()
  let st = stacktrace()
  process.try_call(subject, fn(s) { state.Persist(st, styles, s) }, within: 100)
  |> result.unwrap(class.no_class())
}

pub fn compile_style(styles: List(style.Style), id: String) -> class.Class {
  let Cache(subject) = get_current_cache()
  process.try_call(subject, fn(s) { state.Persist(id, styles, s) }, within: 100)
  |> result.unwrap(class.no_class())
}

pub fn memo(class: class.Class) -> class.Class {
  let Cache(subject) = get_current_cache()
  process.send(subject, state.Memoize(class))
  class
}
