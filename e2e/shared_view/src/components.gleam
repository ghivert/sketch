//// Defines the base components used in the shared view. Think copmonents as in
//// functions that simply returns the HTML correctly formatted.
//// Every component accepts two arrays, attributes and children, to follow the
//// same convention as Lustre standard HTML. That way, you could leverage on
//// your knowledge of Lustre, and behaves exactly as expected.

import sketch
import sketch/lustre/element
import sketch/lustre/element/html
import sketch/size.{px, rem}

/// Standard class defining a card component. Can be used with `sketch.compose`
/// everywhere a card is needed. Properties will be overriden if redefined in a
/// composed class or element.
fn card() {
  sketch.class([
    sketch.display("flex"),
    sketch.flex_direction("column"),
    sketch.border_radius(px(20)),
    sketch.padding(px(20)),
    sketch.font_size(rem(3.0)),
    sketch.font_weight("900"),
  ])
}

/// Standard class defining a card body component. Can be used with `sketch.compose`
/// everywhere a card need a body. Properties will be overriden if redefined in a
/// composed class or element.
fn card_body() {
  sketch.class([
    sketch.flex("1"),
    sketch.font_family("Lexend"),
    sketch.font_weight("600"),
    sketch.font_size(rem(1.0)),
    sketch.line_height("normal"),
  ])
}

fn body_style() {
  sketch.class([
    sketch.line_height("0.75"),
    sketch.display("grid"),
    sketch.gap(px(10)),
    sketch.padding(px(10)),
    sketch.grid_template_columns("repeat(2, 1fr)"),
    sketch.grid_template_areas([
      "topbar topbar", "headline headline", "counter showcase",
    ]),
    sketch.max_width(px(1200)),
    sketch.margin_("auto"),
  ])
}

pub fn body(attrs, children) {
  // demonstrate ability to merge fragment at root
  element.fragment([
    html.div(body_style(), attrs, children),
    html.footer_([], []),
  ])
}

fn topbar_style() {
  sketch.class([
    sketch.display("flex"),
    sketch.grid_area("topbar"),
    sketch.font_size(rem(1.2)),
    sketch.padding_left(px(20)),
    sketch.padding_top(px(5)),
  ])
}

pub fn topbar(attrs, children) {
  html.div(topbar_style(), attrs, children)
}

fn headline_style(value) {
  let background = case value % 2 == 1 {
    True -> "var(--atomic-tangerine)"
    False -> "var(--periwinkle)"
  }
  sketch.class([
    sketch.grid_area("headline"),
    sketch.background(background),
    sketch.text_align("center"),
    sketch.padding(px(120)) |> sketch.important,
    sketch.gap(px(20)),
    sketch.transition("all .2s"),
    sketch.compose(card()),
  ])
}

pub fn headline(value, attrs, children) {
  html.main(headline_style(value), attrs, children)
}

fn headline_subtitle_style() {
  sketch.class([sketch.font_size(rem(1.2)), sketch.font_weight("normal")])
}

pub fn headline_subtitle(attrs, children) {
  html.div(headline_subtitle_style(), attrs, children)
}

fn headline_emphasize_style() {
  sketch.class([sketch.font_size(rem(3.0)), sketch.font_weight("900")])
}

pub fn headline_emphasize(attrs, children) {
  html.div(headline_emphasize_style(), attrs, children)
}

fn counter_style() {
  sketch.class([
    sketch.grid_area("counter"),
    sketch.background("var(--aquamarine)"),
    sketch.compose(card()),
  ])
}

pub fn counter(attrs, children) {
  html.div(counter_style(), attrs, children)
}

pub fn counter_title(attrs, children) {
  html.div_(attrs, children)
}

fn counter_subtitle_style() {
  sketch.class([
    sketch.font_weight("normal"),
    sketch.font_size(rem(0.9)),
    sketch.first_child([sketch.padding_top(px(5))]),
  ])
}

pub fn counter_subtitle(attrs, children) {
  html.div(counter_subtitle_style(), attrs, children)
}

fn button_style() {
  sketch.class([
    sketch.appearance("none"),
    sketch.border("none"),
    sketch.font_family("Lexend"),
    sketch.background("black"),
    sketch.color("white"),
    sketch.border_radius(px(5)),
    sketch.padding_("5px 20px"),
    sketch.font_size(rem(1.2)),
    sketch.font_weight("600"),
    sketch.min_width(px(100)),
    sketch.cursor("pointer"),
    sketch.disabled([sketch.opacity(0.3), sketch.cursor("not-allowed")]),
    sketch.hover([sketch.background("#333")]),
  ])
}

pub fn button(attrs, children) {
  html.button(button_style(), attrs, children)
}

fn value_style() {
  sketch.class([
    sketch.background("var(--turquoise)"),
    sketch.height(size.percent(100)),
    sketch.display("flex"),
    sketch.align_items("center"),
    sketch.justify_content("center"),
    sketch.border_radius(px(5)),
  ])
}

pub fn value(attrs, children) {
  html.div(value_style(), attrs, children)
}

fn value_content_style() {
  sketch.class([sketch.width(size.ch(7)), sketch.text_align("center")])
}

pub fn value_content(attrs, children) {
  html.div(value_content_style(), attrs, children)
}

fn showcase_style() {
  sketch.class([
    sketch.grid_area("showcase"),
    sketch.background("var(--turquoise)"),
    sketch.compose(card()),
  ])
}

pub fn showcase(attrs, children) {
  html.div(showcase_style(), attrs, children)
}

fn counter_body_style() {
  sketch.class([sketch.compose(card_body()), sketch.padding_bottom(px(40))])
}

pub fn counter_body(attrs, children) {
  html.div(counter_body_style(), attrs, children)
}

fn counter_body_title_style() {
  sketch.class([
    sketch.display("flex"),
    sketch.flex_direction("column"),
    sketch.gap(px(20)),
  ])
}

pub fn counter_body_title(attrs, children) {
  html.div(counter_body_title_style(), attrs, children)
}

fn counter_counter_style() {
  sketch.class([
    sketch.display("grid"),
    sketch.align_items("center"),
    sketch.grid_template_columns("repeat(3, auto)"),
    sketch.justify_content("start"),
    sketch.font_size(rem(1.5)),
    sketch.gap(px(10)),
  ])
}

pub fn counter_counter(attrs, children) {
  html.div(counter_counter_style(), attrs, children)
}

fn showcase_body_style() {
  sketch.class([sketch.compose(card_body()), sketch.opacity(0.5)])
}

pub fn showcase_body(attrs, children) {
  html.div(showcase_body_style(), attrs, children)
}

pub fn card_title(attrs, children) {
  html.div_(attrs, children)
}
