//// BEAM only.

@target(erlang)
import gleam/bool
@target(erlang)
import gleam/erlang/process.{type Subject}
@target(erlang)
import gleam/list
@target(erlang)
import gleam/otp/actor
@target(erlang)
import gleam/pair
@target(erlang)
import gleam/result
@target(erlang)
import sketch/error.{type SketchError}
@target(erlang)
import sketch/internals/cache/state
@target(erlang)
import sketch/internals/style

@target(erlang)
/// Manages the styles. Can be instanciated with [`create_cache`](#create_cache).
pub opaque type Cache {
  Persistent(proc: Subject(state.Request))
  Ephemeral(cache: style.Cache)
}

@target(erlang)
pub fn ephemeral() {
  Ephemeral(style.ephemeral())
}

@target(erlang)
pub fn persistent() -> Result(Cache, SketchError) {
  style.persistent()
  |> actor.start(state.loop)
  |> result.map(Persistent)
  |> result.map_error(error.OtpError)
}

@target(erlang)
pub fn render(cache: Cache) -> String {
  case cache {
    Ephemeral(cache:) -> style.render(cache)
    Persistent(proc:) ->
      process.try_call(proc, state.Render, 1000)
      |> result.replace_error(Nil)
      |> result.unwrap("")
  }
}

@target(erlang)
pub fn class_name(class: style.Class, cache: Cache) -> #(Cache, String) {
  case cache {
    Ephemeral(cache:) ->
      style.class_name(class, cache)
      |> pair.map_first(Ephemeral)
    Persistent(proc:) -> {
      let style.Class(string_representation: _, content: c) = class
      use <- bool.guard(when: list.is_empty(c), return: #(cache, ""))
      process.try_call(proc, state.Fetch(class, _), within: 100)
      |> result.unwrap("")
      |> pair.new(cache, _)
    }
  }
}
