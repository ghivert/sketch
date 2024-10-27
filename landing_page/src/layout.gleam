import redraw/html as h
import sketch as s
import sketch/redraw/html as sh
import sketch/size.{px, rem}

pub fn title_container(children) {
  s.class([s.padding(px(36)), s.gap(px(36)), s.margin_bottom(px(60))])
  |> sh.h1([], children)
}

pub fn title(text) {
  s.class([s.line_height("1.6"), s.font_size(rem(2.0)), s.font_weight("600")])
  |> sh.h2([], [h.text(text)])
}

pub fn main_title(text) {
  s.class([s.line_height("1.6"), s.font_size(rem(2.0)), s.font_weight("600")])
  |> sh.div([], [h.text(text)])
}

pub fn row(attributes, children) {
  s.class([s.display("flex"), s.gap(px(12)), s.align_items("end")])
  |> sh.div(attributes, children)
}

pub fn body_container(attributes, children) {
  s.class([
    s.padding(px(36)),
    s.line_height("1.4"),
    s.max_width(px(700)),
    s.display("flex"),
    s.flex_direction("column"),
    s.gap(px(9)),
  ])
  |> sh.div(attributes, children)
}

pub fn section(background, children) {
  s.class([
    s.background(background),
    s.padding(px(36)),
    s.display("flex"),
    s.flex_direction("column"),
    s.gap(px(36)),
  ])
  |> sh.section([], children)
}

pub fn windows_row(children) {
  s.class([s.display("flex"), s.gap(px(36)), s.max_width(px(1000)), s.flex("1")])
  |> sh.div([], children)
}

pub fn windows_wrapper(children) {
  s.class([s.display("flex"), s.justify_content("center")])
  |> sh.div([], children)
}

pub fn buttons_row(children) {
  s.class([
    s.display("flex"),
    s.gap(px(12)),
    s.height(size.percent(100)),
    s.flex_direction("column"),
    s.align_items("center"),
    s.justify_content("center"),
  ])
  |> sh.div([], children)
}
