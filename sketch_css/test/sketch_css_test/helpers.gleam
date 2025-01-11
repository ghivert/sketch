import sketch/css.{type Class}
import sketch_css_test/constants

pub fn card_body(custom: String) -> Class {
  css.class([
    css.background("#ddd"),
    css.background(constants.red),
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
