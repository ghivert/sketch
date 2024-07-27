//// BEAM only.

import gleam/erlang/process.{type Subject}
import gleam/otp/actor
import gleam/result
import sketch/internals/cache/state
import sketch/internals/style

/// Manages the styles. Can be instanciated with [`create_cache`](#create_cache).
pub opaque type Cache {
  Cache(subject: Subject(state.Request))
}

@target(erlang)
pub fn ephemeral() -> Result(Cache, Nil) {
  let assert Ok(subject) = actor.start(style.ephemeral(), state.loop)
  Ok(Cache(subject))
}

@target(erlang)
pub fn persistent() -> Result(Cache, Nil) {
  let assert Ok(subject) = actor.start(style.persistent(), state.loop)
  Ok(Cache(subject))
}

@target(erlang)
pub fn render(cache: Cache) -> String {
  let Cache(subject) = cache
  process.try_call(subject, state.Render, 1000)
  |> result.nil_error()
  |> result.unwrap("")
}

@target(erlang)
pub fn class_name(class: style.Class, cache: Cache) -> String {
  let Cache(subject) = cache
  process.try_call(subject, state.Fetch(class, _), within: 100)
  |> result.unwrap("")
}
