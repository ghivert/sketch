//// BEAM only.

import gleam/erlang/process.{type Subject}
import gleam/otp/actor
import gleam/string
import sketch/internals/style

pub type Request {
  Render(stylesheet: Subject(String))
  Fetch(styles: style.Class, subject: Subject(String))
}

@target(erlang)
pub fn loop(msg: Request, cache: style.Cache) {
  case msg {
    Render(subject) -> {
      process.send(subject, style.render(cache))
      actor.continue(cache)
    }
    Fetch(class, subject) -> {
      let #(cache, class_name) = style.class_name(class, cache)
      process.send(subject, class_name)
      actor.continue(cache)
    }
  }
}
