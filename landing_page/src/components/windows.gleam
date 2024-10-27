import ffi
import redraw/attribute as a
import redraw/html as h
import sketch as s
import sketch/redraw/html as sh
import sketch/size.{px}

const border = "rgb(223, 234, 255)"

pub fn scaffold(children) {
  s.class([
    s.background("rgb(241, 246, 255)"),
    s.border_radius(px(10)),
    s.border("1px solid " <> border),
    s.overflow("hidden"),
    s.flex("1"),
    s.display("flex"),
    s.flex_direction("column"),
  ])
  |> sh.div([], children)
}

pub fn render(children) {
  s.class([
    s.background("rgb(241, 246, 255)"),
    s.border_radius(px(10)),
    s.border("1px solid " <> border),
    s.overflow("hidden"),
    s.flex("1"),
    s.margin(px(24)),
  ])
  |> sh.div([], children)
}

pub fn menu_bar(children) {
  s.class([])
  |> sh.div([], children)
}

pub fn traffic_lights() {
  s.class([
    s.display("flex"),
    s.gap(px(5)),
    s.padding(px(10)),
    s.border_bottom("1px solid " <> border),
  ])
  |> sh.div([], [
    traffic_light(Red),
    traffic_light(Orange),
    traffic_light(Green),
  ])
}

type Traffic {
  Red
  Orange
  Green
}

fn traffic_light(color) {
  let color = case color {
    Red -> "rgb(255, 95, 87)"
    Orange -> "rgb(254, 188, 46)"
    Green -> "rgb(40, 202, 65)"
  }
  s.class([
    s.width(px(10)),
    s.height(px(10)),
    s.background(color),
    s.border_radius(px(5)),
  ])
  |> sh.div([], [])
}

pub fn editor(content) {
  let content = a.inner_html(ffi.highlight(content))
  s.class([
    s.background("#fff"),
    s.padding(px(10)),
    s.display("flex"),
    s.max_height(px(400)),
    s.overflow("auto"),
    s.flex_grow(1),
  ])
  |> sh.div([], [h.code([a.dangerously_set_inner_html(content)], [])])
}

pub fn css(content) {
  let content = a.inner_html(ffi.highlight_css(content))
  s.class([
    s.background("#fff"),
    s.padding(px(10)),
    s.display("flex"),
    s.max_height(px(400)),
    s.overflow("auto"),
    s.flex_grow(1),
  ])
  |> sh.div([], [h.code([a.dangerously_set_inner_html(content)], [])])
}

pub fn html(content) {
  let content = a.inner_html(ffi.highlight_xml(content))
  s.class([
    s.background("#fff"),
    s.padding(px(10)),
    s.display("flex"),
    s.max_height(px(400)),
    s.overflow("auto"),
    s.flex_grow(1),
  ])
  |> sh.div([], [h.code([a.dangerously_set_inner_html(content)], [])])
}
