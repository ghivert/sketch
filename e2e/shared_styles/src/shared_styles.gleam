import sketch/css
import sketch/css/length.{ch, percent, px, rem}

/// Standard class defining a card component. Can be used with `css.compose`
/// everywhere a card is needed. Properties will be overriden if redefined in a
/// composed class or element.
fn card() {
  css.class([
    css.display("flex"),
    css.flex_direction("column"),
    css.border_radius(px(20)),
    css.padding(px(20)),
    css.font_size(rem(3.0)),
    css.font_weight("900"),
  ])
}

/// Standard class defining a card body component. Can be used with `css.compose`
/// everywhere a card need a body. Properties will be overriden if redefined in a
/// composed class or element.
fn card_body() {
  css.class([
    css.flex("1"),
    css.font_family("Lexend"),
    css.font_weight("600"),
    css.font_size(rem(1.0)),
    css.line_height("normal"),
  ])
}

pub fn body_style() {
  css.class([
    css.line_height("0.75"),
    css.display("grid"),
    css.gap(px(10)),
    css.padding(px(10)),
    css.grid_template_columns("repeat(2, 1fr)"),
    css.grid_template_areas([
      "topbar topbar", "headline headline", "counter showcase",
    ]),
    css.max_width(px(1200)),
    css.margin_("auto"),
  ])
}

pub fn topbar_style() {
  css.class([
    css.display("flex"),
    css.grid_area("topbar"),
    css.font_size(rem(1.2)),
    css.padding_left(px(20)),
    css.padding_top(px(5)),
  ])
}

pub fn headline_style(value) {
  let background = case value % 2 == 1 {
    True -> "var(--atomic-tangerine)"
    False -> "var(--periwinkle)"
  }
  css.class([
    css.grid_area("headline"),
    css.background(background),
    css.text_align("center"),
    css.padding(px(120)) |> css.important,
    css.gap(px(20)),
    css.transition("all .2s"),
    css.compose(card()),
  ])
}

pub fn headline_subtitle_style() {
  css.class([css.font_size(rem(1.2)), css.font_weight("normal")])
}

pub fn headline_emphasize_style() {
  css.class([css.font_size(rem(3.0)), css.font_weight("900")])
}

pub fn counter_style() {
  css.class([
    css.grid_area("counter"),
    css.background("var(--aquamarine)"),
    css.compose(card()),
  ])
}

pub fn counter_subtitle_style() {
  css.class([
    css.font_weight("normal"),
    css.font_size(rem(0.9)),
    css.first_child([css.padding_top(px(5))]),
  ])
}

pub fn button_style() {
  css.class([
    css.appearance("none"),
    css.border("none"),
    css.font_family("Lexend"),
    css.background("black"),
    css.color("white"),
    css.border_radius(px(5)),
    css.padding_("5px 20px"),
    css.font_size(rem(1.2)),
    css.font_weight("600"),
    css.min_width(px(100)),
    css.cursor("pointer"),
    css.disabled([css.opacity(0.3), css.cursor("not-allowed")]),
    css.hover([css.background("#333")]),
  ])
}

pub fn value_style() {
  css.class([
    css.background("var(--turquoise)"),
    css.height(percent(100)),
    css.display("flex"),
    css.align_items("center"),
    css.justify_content("center"),
    css.border_radius(px(5)),
  ])
}

pub fn value_content_style() {
  css.class([css.width(ch(7.0)), css.text_align("center")])
}

pub fn showcase_style() {
  css.class([
    css.grid_area("showcase"),
    css.background("var(--turquoise)"),
    css.compose(card()),
  ])
}

pub fn counter_body_style() {
  css.class([css.compose(card_body()), css.padding_bottom(px(40))])
}

pub fn counter_body_title_style() {
  css.class([css.display("flex"), css.flex_direction("column"), css.gap(px(20))])
}

pub fn counter_counter_style() {
  css.class([
    css.display("grid"),
    css.align_items("center"),
    css.grid_template_columns("repeat(3, auto)"),
    css.justify_content("start"),
    css.font_size(rem(1.5)),
    css.gap(px(10)),
  ])
}

pub fn showcase_body_style() {
  css.class([css.compose(card_body()), css.opacity(0.5)])
}
