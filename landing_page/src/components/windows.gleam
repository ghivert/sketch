import ffi
import redraw/dom/attribute as a
import redraw/dom/html as h
import sketch/css
import sketch/css/length.{percent, px}
import sketch/redraw/dom/html as sh

pub fn scaffold(children) {
  css.class([
    css.background("var(--window-bg)"),
    css.border_radius(px(10)),
    css.border("1px solid var(--window-border)"),
    css.overflow("hidden"),
    css.flex("1"),
    css.display("flex"),
    css.flex_direction("column"),
    css.max_height(px(400)),
    css.max_width(percent(100)),
  ])
  |> sh.div([], children)
}

pub fn render(children) {
  css.class([
    css.background("var(--window-bg)"),
    css.border_radius(px(10)),
    css.border("1px solid var(--window-border)"),
    css.overflow("hidden"),
    css.flex("1"),
    css.margin(px(24)),
  ])
  |> sh.div([], children)
}

pub fn menu_bar(children) {
  css.class([])
  |> sh.div([], children)
}

pub fn traffic_lights() {
  css.class([
    css.display("flex"),
    css.gap(px(5)),
    css.padding(px(10)),
    css.border_bottom("1px solid var(--window-border)"),
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
  css.class([
    css.width(px(10)),
    css.height(px(10)),
    css.background(color),
    css.border_radius(px(5)),
  ])
  |> sh.div([], [])
}

pub fn editor(content) {
  let content = a.inner_html(ffi.highlight(content))
  css.class([
    css.background("var(--background)"),
    css.padding(px(10)),
    css.display("flex"),
    css.overflow("auto"),
    css.flex_grow(1),
  ])
  |> sh.div([], [h.code([a.dangerously_set_inner_html(content)], [])])
}

pub fn css(content) {
  let content = a.inner_html(ffi.highlight_css(content))
  css.class([
    css.background("var(--background)"),
    css.padding(px(10)),
    css.display("flex"),
    css.max_height(px(400)),
    css.overflow("auto"),
    css.flex_grow(1),
  ])
  |> sh.div([], [h.code([a.dangerously_set_inner_html(content)], [])])
}

pub fn html(content) {
  let content = a.inner_html(ffi.highlight_xml(content))
  css.class([
    css.background("var(--background)"),
    css.padding(px(10)),
    css.display("flex"),
    css.max_height(px(400)),
    css.overflow("auto"),
    css.flex_grow(1),
  ])
  |> sh.div([], [h.code([a.dangerously_set_inner_html(content)], [])])
}
