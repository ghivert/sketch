//// BEAM only.

import gleam/erlang/process.{type Subject}
@target(erlang)
import gleam/otp/actor
import sketch/internals/style

pub type Request {
  Render(response: Subject(String))
  Fetch(class: style.Class, response: Subject(String))
}

@target(erlang)
pub fn loop(msg: Request, cache: style.Cache) -> actor.Next(a, style.Cache) {
  case msg {
    Render(response:) -> {
      process.send(response, style.render(cache))
      actor.continue(cache)
    }
    Fetch(class:, response:) -> {
      let #(cache, class_name) = style.class_name(class, cache)
      process.send(response, class_name)
      actor.continue(cache)
    }
  }
}
