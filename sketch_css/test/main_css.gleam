import helper
import sketch/css
import sketch/css.{background, class as t} as s
import sketch/css/angle
import sketch/css/length.{percent, px}
import sketch/css/media
import sketch/css/transform

pub fn card(custom) {
  let red = "red"
  css.class([
    css.background("#ddd"),
    css.z_index(100),
    css.opacity(1.0),
    css.width(percent(100)),
    background("red"),
    s.display("block"),
    s.padding(length.pt(12)),
    s.padding(px(12)),
    s.font_weight("900"),
    helper.custom_color(custom),
    s.grid_template_areas(["header", "main"]),
    s.property("--example-property", "example-value"),
    s.hover([s.background(custom)]),
    s.media(media.max_width(px(700)), [s.background("blue")]),
    s.media(media.max_width(px(700)) |> media.or(media.min_width(px(600))), [
      s.background("blue"),
    ]),
    s.media(media.max_width(px(700)) |> media.or(media.min_width(400 |> px)), [
      s.background("blue"),
      s.hover([s.background(red)]),
      s.hover([
        s.child(block(custom), [
          s.background("blue"),
          s.hover([s.background(red)]),
        ]),
      ]),
    ]),
    s.child(block(custom), [
      s.background("red"),
      s.hover([s.background("blue")]),
    ]),
  ])
}

pub fn block(custom) {
  css.class([
    css.background("#ccc"),
    css.display("flex") |> css.important,
    css.compose(helper.card_body(custom)),
    s.transform([
      transform.rotate(angle.deg(90.0)),
      transform.rotate(angle.deg(90.0)),
    ]),
  ])
}

pub fn body(custom) {
  css.class([css.compose(card(custom)), css.background("#ccc") |> css.important])
}

pub fn main() {
  t([css.background("#ddd")])
}
