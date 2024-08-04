import sketch
import sketch.{background, class as t} as s
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
