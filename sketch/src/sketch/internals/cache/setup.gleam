//// BEAM only.

import gleam/bool
import gleam/erlang/process.{type Subject}
import gleam/list
import gleam/otp/actor
import gleam/pair
import gleam/result
import sketch/internals/cache/state
import sketch/internals/style

/// Manages the styles. Can be instanciated with [`create_cache`](#create_cache).
pub opaque type Cache {
  PersistentCache(subject: Subject(state.Request))
  EphemeralCache(cache: style.Cache)
}

@target(erlang)
pub fn ephemeral() {
  EphemeralCache(style.ephemeral())
}

@target(erlang)
pub fn persistent() -> Result(Cache, Nil) {
  let assert Ok(subject) = actor.start(style.persistent(), state.loop)
  Ok(PersistentCache(subject))
}

@target(erlang)
pub fn render(cache: Cache) -> String {
  case cache {
    EphemeralCache(cache:) -> style.render(cache)
    PersistentCache(subject:) -> {
      process.try_call(subject, state.Render, 1000)
      |> result.nil_error()
      |> result.unwrap("")
    }
  }
}

@target(erlang)
pub fn class_name(class: style.Class, cache: Cache) -> #(Cache, String) {
  case cache {
    EphemeralCache(cache:) -> {
      let #(cache, class_name) = style.class_name(class, cache)
      #(EphemeralCache(cache:), class_name)
    }
    PersistentCache(subject:) -> {
      let style.Class(string_representation: _, content: c) = class
      use <- bool.guard(when: list.is_empty(c), return: #(cache, ""))
      process.try_call(subject, state.Fetch(class, _), within: 100)
      |> result.unwrap("")
      |> pair.new(cache, _)
    }
  }
}
