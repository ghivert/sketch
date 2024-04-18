//// Functions here only work for the BEAM target. JS has a native implementation,
//// to increase performances.

import gleam/erlang/process.{type Pid, type Subject}
import gleam/io
import gleam/option.{None}
import gleam/otp/actor
import sketch/error
import sketch/internals/style
import sketch/options.{type Options}

/// Manages the styles. Can be instanciated with [`create_cache`](#create_cache).
pub opaque type Cache {
  Cache(subject: Subject(Msg))
}

pub opaque type Class {
  Class
}

@external(erlang, "sketch_ffi", "save_current_cache")
fn save_current_cache(cache: Cache) -> Nil

@external(erlang, "sketch_ffi", "get_current_cache")
fn get_current_cache() -> Cache

type State {
  State
}

type Msg {
  Msg
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
  let assert Ok(dispatch) = actor.start(State, update_cache)
  io.debug(dispatch)
  Ok(Cache(dispatch))
}

pub fn prepare(cache: Cache) -> Nil {
  save_current_cache(cache)
}

pub fn render(_cache: Cache) -> Nil {
  Nil
}

fn update_cache(msg: Msg, state: State) {
  case msg {
    Msg -> {
      io.debug("There ?")
      actor.continue(state)
    }
  }
}

pub fn compile_class(styles: List(style.Style)) -> Class {
  io.debug("class")
  io.debug(styles)
  let Cache(subject) = get_current_cache()
  process.send(subject, Msg)
  Class
}

pub fn compile_style(styles: List(style.Style), id: String) -> Class {
  io.debug("style")
  io.debug(styles)
  io.debug(id)
  // io.debug(get_current_cache())
  Class
}

pub fn memo(class: Class) -> Class {
  class
}
