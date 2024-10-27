import redraw/html as h
import sketch as s
import sketch/redraw/html as sh
import sketch/size.{px, rem}

pub fn primary(content: String) {
  s.class([
    s.background("#eee"),
    s.border_radius(px(6)),
    s.padding_("9px 16px"),
    s.font_size(rem(1.0)),
  ])
  |> sh.button([], [h.text(content)])
}

pub type Color {
  Red
  Orange
  Green
}

pub fn example(color, text) {
  let background = case color {
    Red -> "rgb(255, 95, 87)"
    Orange -> "rgb(254, 188, 46)"
    Green -> "rgb(40, 202, 65)"
  }
  s.class([
    s.background(background),
    s.color("white"),
    s.border_radius(px(8)),
    s.transition("all .3s"),
    s.border("none"),
    s.appearance("none"),
    s.font_family("inherit"),
    s.font_size(px(16)),
    s.padding(px(12)),
    s.font_weight("bold"),
    s.cursor("pointer"),
    s.min_width(px(200)),
    s.text_align("center"),
    s.hover([s.opacity(0.7)]),
  ])
  |> sh.button([], [h.text(text)])
}
