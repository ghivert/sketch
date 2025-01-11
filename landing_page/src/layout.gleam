import redraw/dom/attribute as a
import redraw/dom/html as h
import sketch/css
import sketch/css/length.{percent, px, rem}
import sketch/css/media
import sketch/redraw/dom/html as sh

fn max_width() {
  css.class([
    css.max_width(px(1500)),
    css.margin_("auto"),
    css.width(percent(100)),
    css.padding(px(36)),
    css.display("inherit"),
    css.flex_direction("inherit"),
    css.gap_("inherit"),
  ])
}

pub fn width_container(children) {
  sh.div(max_width(), [], children)
}

pub fn title_container(children) {
  css.class([css.compose(max_width()), css.gap(px(36)), css.display("flex")])
  |> sh.h1([], children)
}

pub fn title_container_inside(children) {
  css.class([css.gap(px(36))])
  |> sh.div([], children)
}

pub fn title(text) {
  css.class([
    css.line_height("1.6"),
    css.font_size(rem(2.0)),
    css.font_weight("600"),
  ])
  |> sh.h2([], [h.text(text)])
}

pub fn main_title(text) {
  css.class([
    css.line_height("1.6"),
    css.font_size(rem(2.0)),
    css.font_weight("600"),
  ])
  |> sh.div([], [h.text(text)])
}

pub fn row(attributes, children) {
  css.class([
    css.display("flex"),
    css.gap(px(12)),
    css.align_items("end"),
    css.flex_wrap("wrap"),
  ])
  |> sh.div(attributes, children)
}

pub fn column(attributes, children) {
  css.class([
    css.display("flex"),
    css.gap(px(12)),
    css.align_items("start"),
    css.flex_direction("column"),
  ])
  |> sh.div(attributes, children)
}

pub fn body_container(attributes, children) {
  css.class([
    css.line_height("1.4"),
    css.max_width(px(700)),
    css.display("flex"),
    css.flex_direction("column"),
    css.gap(px(9)),
  ])
  |> sh.div(attributes, children)
}

pub fn section(id, background, children) {
  css.class([
    css.background(background),
    css.display("flex"),
    css.flex_direction("column"),
    css.gap(px(36)),
  ])
  |> sh.section([a.id(id)], children)
}

pub fn section_explanation(attributes, children) {
  css.class([css.max_width(px(400)), css.line_height("1.4")])
  |> sh.div(attributes, children)
}

pub fn windows_row(children) {
  css.class([
    css.display("flex"),
    css.gap(px(36)),
    css.max_width(px(1000)),
    css.flex("1"),
    css.media(media.max_width(px(800)), [
      css.flex_direction("column"),
      css.align_items("center"),
    ]),
  ])
  |> sh.div([], children)
}

pub fn windows_wrapper(breakpoint, children) {
  css.class([
    css.display("flex"),
    css.flex_direction("row"),
    css.gap(px(36)),
    css.media(media.max_width(breakpoint), [css.flex_direction("column")]),
  ])
  |> sh.div([], children)
}

pub fn buttons_row(children) {
  css.class([
    css.display("flex"),
    css.gap(px(12)),
    css.height(percent(100)),
    css.flex_direction("column"),
    css.align_items("center"),
    css.justify_content("center"),
    css.media(media.max_width(px(800)), [css.padding(px(36))]),
  ])
  |> sh.div([], children)
}
