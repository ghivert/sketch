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
import sketch/internals/cache/cache

@target(erlang)
/// Manages the styles. Can be instanciated with [`create_cache`](#create_cache).
pub opaque type Cache {
  Persistent(proc: Subject(Request))
  Ephemeral(cache: cache.Cache)
}

@target(erlang)
pub fn ephemeral() {
  Ephemeral(cache: cache.new())
}

@target(erlang)
pub fn persistent() -> Result(Cache, SketchError) {
  cache.new()
  |> actor.start(loop)
  |> result.map(Persistent)
  |> result.map_error(error.OtpError)
}

@target(erlang)
pub fn render(cache: Cache) -> String {
  case cache {
    Ephemeral(cache:) -> cache.render_sheet(cache)
    Persistent(proc:) ->
      process.try_call(proc, Render, 1000)
      |> result.replace_error(Nil)
      |> result.unwrap("")
  }
}

@target(erlang)
pub fn class_name(class: cache.Class, cache: Cache) -> #(Cache, String) {
  case cache {
    Ephemeral(cache:) ->
      cache.class_name(class, cache)
      |> pair.map_first(Ephemeral)
    Persistent(proc:) -> {
      use <- bool.guard(when: list.is_empty(class.content), return: #(cache, ""))
      process.try_call(proc, Fetch(class, _), within: 100)
      |> result.unwrap("")
      |> pair.new(cache, _)
    }
  }
}

@target(erlang)
pub fn at_rule(rule: cache.AtRule, cache: Cache) -> Cache {
  case cache {
    Ephemeral(cache:) -> Ephemeral(cache.at_rule(rule, cache))
    Persistent(proc:) -> {
      let _ = process.try_call(proc, Push(rule, _), within: 100)
      cache
    }
  }
}

@target(erlang)
pub type Request {
  Render(response: Subject(String))
  Fetch(class: cache.Class, response: Subject(String))
  Push(rule: cache.AtRule, response: Subject(Nil))
}

@target(erlang)
pub fn loop(msg: Request, cache: cache.Cache) -> actor.Next(a, cache.Cache) {
  case msg {
    Render(response:) -> {
      process.send(response, cache.render_sheet(cache))
      actor.continue(cache)
    }
    Fetch(class:, response:) -> {
      let #(cache, class_name) = cache.class_name(class, cache)
      process.send(response, class_name)
      actor.continue(cache)
    }
    Push(rule:, response:) -> {
      let cache = cache.at_rule(rule, cache)
      process.send(response, Nil)
      actor.continue(cache)
    }
  }
}
