import sketch/css
import sketch/css/length

pub fn important() {
  css.class([
    css.compose(content()),
    css.color("blue"),
    css.background("#ccc") |> css.important,
  ])
}

fn content() {
  css.class([
    css.background("red"),
    css.color("red"),
    css.padding(length.px(12)),
  ])
}
