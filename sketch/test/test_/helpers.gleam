import birdie
import gleam/string
import gleeunit/should
import sketch
import sketch/css.{type Class}

pub fn card_body(custom: String) -> Class {
  css.class([
    css.background("#ddd"),
    css.background("red"),
    css.display("block"),
    custom_color(custom),
  ])
}

pub fn custom_color(custom: String) -> css.Style {
  css.color(custom)
}

@target(erlang)
pub fn multitarget_title(title: String) {
  "erlang_" <> title
}

@target(javascript)
pub fn multitarget_title(title: String) {
  "js_" <> title
}

pub fn compute_class(class: css.Class, title: String) {
  let assert Ok(stylesheet) = sketch.stylesheet(strategy: sketch.Ephemeral)
  let #(stylesheet, class_name) = sketch.class_name(class, stylesheet)
  let content = sketch.render(stylesheet)
  content |> string.contains(class_name) |> should.be_true
  birdie.snap(title: multitarget_title(title), content:)
}
