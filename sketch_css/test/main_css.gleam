import sketch
import sketch.{background, class as t} as s
import sketch/media
import sketch/size.{px}

fn custom_color(custom) {
  sketch.color(custom)
}

pub fn card(custom) {
  sketch.class([
    sketch.background("#ddd"),
    background("red"),
    s.display("block"),
    s.padding(size.pt(12)),
    s.padding(px(12)),
    custom_color(custom),
    s.grid_template_areas(["muf", "muf"]),
    s.property("muf", "mumuf"),
    s.hover([s.background(custom)]),
    s.media(media.max_width(px(700)), [s.background("blue")]),
    s.media(media.max_width(px(700)) |> media.or(media.min_width(px(600))), [
      s.background("blue"),
    ]),
    s.media(media.max_width(px(700)) |> media.or(media.min_width(400 |> px)), [
      s.background("blue"),
      s.hover([s.background("red")]),
    ]),
  ])
}

pub fn card_body(custom) {
  sketch.class([
    sketch.background("#ddd"),
    background("red"),
    s.display("block"),
    custom_color(custom),
  ])
}

pub fn block(custom) {
  sketch.class([
    sketch.background("#ccc"),
    sketch.display("flex"),
    sketch.compose(card(custom)),
  ])
}

pub fn body(custom) {
  sketch.class([
    sketch.compose(card(custom)),
    sketch.background("#ccc") |> sketch.important,
  ])
}

pub fn main() {
  t([sketch.background("#ddd")])
}
