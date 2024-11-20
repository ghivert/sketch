import redraw
import redraw/attribute as a
import redraw/html as h
import sketch as s
import sketch/redraw/html as sh
import sketch/size.{px, rem}

pub fn footer() {
  use <- redraw.component__("Footer")
  footer_([], [
    column([
      footer_details([h.text("Made with 💜 by Chou Corp.")]),
      footer_details([
        h.text("With the help of "),
        link("https://gaspard.design", "gaspard.design"),
      ]),
    ]),
  ])
}

fn footer_(attributes, children) {
  s.class([
    s.margin_top(px(120)),
    s.padding(px(36)),
    s.display("flex"),
    s.justify_content("center"),
  ])
  |> sh.div(attributes, children)
}

fn footer_details(children) {
  s.class([
    s.font_size(rem(0.8)),
    s.line_height("1.4"),
    s.font_weight("500"),
    s.color("var(--text-grey)"),
  ])
  |> sh.div([], children)
}

fn column(children) {
  s.class([
    s.display("flex"),
    s.flex_direction("column"),
    s.align_items("center"),
  ])
  |> sh.div([], children)
}

fn link(href, content) {
  s.class([s.color("#ffaff3")])
  |> sh.a([a.href(href)], [h.text(content)])
}
