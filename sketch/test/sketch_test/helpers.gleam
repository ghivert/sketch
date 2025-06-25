import birdie
import gleam/string
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
  assert string.contains(content, class_name)
  birdie.snap(title: multitarget_title(title), content:)
  Nil
}

pub fn compute_at_rule(rule: css.AtRule, title: String) {
  let assert Ok(stylesheet) = sketch.stylesheet(strategy: sketch.Ephemeral)
  let stylesheet = sketch.at_rule(rule, stylesheet)
  let content = sketch.render(stylesheet)
  birdie.snap(title: multitarget_title(title), content:)
  Nil
}
