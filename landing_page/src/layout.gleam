import redraw/attribute as a
import redraw/html as h
import sketch as s
import sketch/media
import sketch/redraw/html as sh
import sketch/size.{percent, px, rem}

fn max_width() {
  s.class([
    s.max_width(px(1500)),
    s.margin_("auto"),
    s.width(percent(100)),
    s.padding(px(36)),
    s.display("inherit"),
    s.flex_direction("inherit"),
    s.gap_("inherit"),
  ])
}

pub fn width_container(children) {
  sh.div(max_width(), [], children)
}

pub fn title_container(children) {
  s.class([s.compose(max_width()), s.gap(px(36)), s.display("flex")])
  |> sh.h1([], children)
}

pub fn title_container_inside(children) {
  s.class([s.gap(px(36))])
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
  s.class([
    s.display("flex"),
    s.gap(px(12)),
    s.align_items("end"),
    s.flex_wrap("wrap"),
  ])
  |> sh.div(attributes, children)
}

pub fn column(attributes, children) {
  s.class([
    s.display("flex"),
    s.gap(px(12)),
    s.align_items("start"),
    s.flex_direction("column"),
  ])
  |> sh.div(attributes, children)
}

pub fn body_container(attributes, children) {
  s.class([
    s.line_height("1.4"),
    s.max_width(px(700)),
    s.display("flex"),
    s.flex_direction("column"),
    s.gap(px(9)),
  ])
  |> sh.div(attributes, children)
}

pub fn section(id, background, children) {
  s.class([
    s.background(background),
    s.display("flex"),
    s.flex_direction("column"),
    s.gap(px(36)),
  ])
  |> sh.section([a.id(id)], children)
}

pub fn section_explanation(attributes, children) {
  s.class([s.max_width(px(400)), s.line_height("1.4")])
  |> sh.div(attributes, children)
}

pub fn windows_row(children) {
  s.class([
    s.display("flex"),
    s.gap(px(36)),
    s.max_width(px(1000)),
    s.flex("1"),
    s.media(media.max_width(px(800)), [
      s.flex_direction("column"),
      s.align_items("center"),
    ]),
  ])
  |> sh.div([], children)
}

pub fn windows_wrapper(breakpoint, children) {
  s.class([
    s.display("flex"),
    s.flex_direction("row"),
    s.gap(px(36)),
    s.media(media.max_width(breakpoint), [s.flex_direction("column")]),
  ])
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
    s.media(media.max_width(px(800)), [s.padding(px(36))]),
  ])
  |> sh.div([], children)
}
