import sketch/css
import sketch/css/length.{px}
import sketch/css/media

pub fn example(custom) {
  css.class([
    css.media(media.or(media.max_width(px(700)), media.min_width(px(400))), [
      css.background("blue"),
      css.hover([css.background("red")]),
      css.hover([
        css.child(content(custom), [
          css.background("blue"),
          css.hover([css.background("red")]),
        ]),
      ]),
    ]),
    css.child(content(custom), [
      css.background("red"),
      css.hover([css.background("blue")]),
    ]),
  ])
}

pub fn content(custom) {
  css.class([
    css.color(custom),
    css.background("green"),
    css.padding(length.px(12)),
  ])
}
