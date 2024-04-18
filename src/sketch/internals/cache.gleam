//// Functions here only work for the BEAM target. JS has a native implementation,
//// to increase performances.

import gleam/erlang/process.{type Subject}
import gleam/function
import gleam/io
import gleam/otp/actor
import gleam/pair
import gleam/result
import sketch/error
import sketch/internals/class
import sketch/internals/state
import sketch/internals/style
import sketch/options.{type Options}

/// Manages the styles. Can be instanciated with [`create_cache`](#create_cache).
pub opaque type Cache {
  Cache(subject: Subject(Request))
}

@external(erlang, "sketch_ffi", "save_current_cache")
fn save_current_cache(cache: Cache) -> Nil

@external(erlang, "sketch_ffi", "get_current_cache")
fn get_current_cache() -> Cache

@external(erlang, "sketch_ffi", "stacktrace")
fn stacktrace() -> String

type Request {
  Prepare
  Diff(stylesheet: Subject(String))
  Memoize(class: class.Class)
  Persist(
    class_id: String,
    styles: List(style.Style),
    subject: Subject(class.Class),
  )
}

/// Create a cache manager, managing the styles for every repaint. You can
/// instanciate as much cache manager that you want, if you want to use multiple
/// render lifecycle.
/// You can output the styles directly in a node style in the DOM, or by pushing
/// them directly in a CSSStyleSheet, at the document level. The choice is up to
/// you at the initialization of the Cache.
/// If you're using Lustre, you shouldn't have to worry about it, and consider
/// it as internal low-level.
@external(javascript, "../../cache.ffi.mjs", "createCache")
pub fn create_cache(_options: Options) -> Result(Cache, error.SketchError) {
  let assert Ok(subject) = actor.start(state.init(), update_cache)
  Ok(Cache(subject))
}

pub fn prepare(cache: Cache) -> Nil {
  let Cache(subject) = cache
  save_current_cache(cache)
  io.debug("PREPARE RRSTIENTSUIRERSIUTENSRTUNSRIETNRSUTENRUTSIETSRET")
  process.send(subject, Prepare)
}

pub fn render(cache: Cache) -> String {
  let Cache(subject) = cache
  let res = process.try_call(subject, Diff, 1000)
  result.unwrap(res, "")
}

fn update_cache(msg: Request, state: state.State) {
  case msg {
    Prepare ->
      state
      |> state.prepare()
      |> actor.continue()
    Diff(subject) ->
      state
      |> state.diff()
      |> function.tap(fn(s) { process.send(subject, state.render(s)) })
      |> actor.continue()
    Memoize(class) ->
      state
      |> state.memo(class)
      |> actor.continue()
    Persist(id, styles, subject) -> {
      result.unwrap_both({
        let res = state.persist(state, id, styles)
        use _ <- result.map_error(res)
        state.compute_class(state, id, styles)
      })
      |> function.tap(fn(state) { process.send(subject, pair.second(state)) })
      |> pair.first()
      |> actor.continue()
    }
  }
}

pub fn compile_class(styles: List(style.Style)) -> class.Class {
  let Cache(subject) = get_current_cache()
  let st = stacktrace()
  process.try_call(subject, fn(s) { Persist(st, styles, s) }, within: 100)
  |> result.unwrap(class.no_class())
}

pub fn compile_style(styles: List(style.Style), id: String) -> class.Class {
  let Cache(subject) = get_current_cache()
  process.try_call(subject, fn(s) { Persist(id, styles, s) }, within: 100)
  |> result.unwrap(class.no_class())
}

pub fn memo(class: class.Class) -> class.Class {
  let Cache(subject) = get_current_cache()
  process.send(subject, Memoize(class))
  class
}
