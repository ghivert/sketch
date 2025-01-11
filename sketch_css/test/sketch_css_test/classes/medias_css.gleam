import sketch/css
import sketch/css/length.{px}
import sketch/css/media

pub fn simple() {
  css.class([
    css.background("blue"),
    css.media(media.max_width(px(700)), [
      css.background("red"),
      css.background("blue"),
    ]),
  ])
}

pub fn and() {
  css.class([
    css.background("blue"),
    css.media(media.max_width(px(700)) |> media.and(media.min_width(px(600))), [
      css.background("red"),
      css.background("blue"),
    ]),
  ])
}

pub fn or() {
  css.class([
    css.background("blue"),
    css.media(media.max_width(px(700)) |> media.or(media.min_width(px(600))), [
      css.background("red"),
      css.background("blue"),
    ]),
  ])
}

pub fn and_or() {
  css.class([
    css.background("blue"),
    css.media(
      media.max_width(px(700))
        |> media.and(media.min_width(px(700)))
        |> media.or(media.min_width(px(600))),
      [css.background("red"), css.background("blue")],
    ),
  ])
}

pub fn pseudo_class() {
  css.class([
    css.background("blue"),
    css.media(media.max_width(px(700)), [
      css.background("red"),
      css.background("blue"),
      css.hover([css.background("green")]),
    ]),
  ])
}
