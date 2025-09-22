import redraw/dom/attribute as a
import redraw/dom/html as h
import sketch/css
import sketch/css/length.{px, rem}
import sketch/redraw/dom/hooks/html as sh

fn primary_class() {
  css.class([
    css.display("block"),
    css.text_decoration("none"),
    css.background("var(--dark-background)"),
    css.border_radius(px(6)),
    css.padding_("9px 16px"),
    css.font_size(rem(1.0)),
    css.color("inherit"),
  ])
}

pub fn primary(attributes, content: String) {
  use <- sh.button(attributes, [h.text(content)])
  primary_class()
}

pub type Color {
  Red
  Orange
  Green
}

pub fn example(color, text) {
  use <- sh.button([], [h.text(text)])
  let background = case color {
    Red -> "rgb(255, 95, 87)"
    Orange -> "rgb(254, 188, 46)"
    Green -> "rgb(40, 202, 65)"
  }
  css.class([
    css.background(background),
    css.color("white"),
    css.border_radius(px(8)),
    css.transition("all .3s"),
    css.border("none"),
    css.appearance("none"),
    css.font_family("inherit"),
    css.font_size(px(16)),
    css.padding(px(12)),
    css.font_weight("bold"),
    css.cursor("pointer"),
    css.min_width(px(200)),
    css.text_align("center"),
    css.hover([css.opacity(0.7)]),
  ])
}

pub fn link(link, content) {
  use <- sh.a([a.href(link)], [h.text(content)])
  css.class([
    css.compose(primary_class()),
    css.background("var(--background)"),
    css.border("1px solid var(--border-color)"),
  ])
}
