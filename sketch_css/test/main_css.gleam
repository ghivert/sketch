import glance.{type Field as F, UnlabelledField as U} as g
import helper
import sketch/css
import sketch/css.{type Class as Classs, background, class as t} as s
import sketch/css/angle
import sketch/css/media
import sketch/css/size.{percent, px}
import sketch/css/transform

pub type Example =
  Classs

pub type Example2(a) {
  Example2(Example, Classs, s.Class, css.Class, var: a)
}

pub type Example3 {
  Example3(var: Int)
  // U(String)
}

pub const bloup: List(Classs) = []

pub const bloupie = g.UnlabelledField("")

pub const bloupie_2: F(String) = U("")

pub fn card(custom) {
  css.class([
    css.background("#ddd"),
    css.z_index(100),
    css.opacity(1.0),
    css.width(percent(100)),
    background("red"),
    s.display("block"),
    s.padding(size.pt(12)),
    s.padding(px(12)),
    s.font_weight("900"),
    helper.custom_color(custom),
    s.grid_template_areas(["muf mumuf", "muf mumuf"]),
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
    s.child(block(custom), [s.background("red")]),
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
