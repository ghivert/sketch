import sketch/css.{type Class, background}

pub fn card_body(custom: String) -> Class {
  css.class([
    css.background("#ddd"),
    background("red"),
    css.display("block"),
    custom_color(custom),
  ])
}

pub fn custom_color(custom: String) -> css.Style {
  css.color(custom)
}
