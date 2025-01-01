import redraw
import redraw/dom/attribute as a
import redraw/dom/html as h
import sketch/css
import sketch/css/size.{px, rem}
import sketch/redraw/html as sh

pub fn footer() {
  use <- redraw.component__("Footer")
  footer_([], [
    column([
      footer_details([h.text("Made with 💜 at Chou Corp.")]),
      footer_details([
        h.text("With the help of "),
        link("https://gaspardbuffet.com", "Gaspard Buffet"),
      ]),
    ]),
  ])
}

fn footer_(attributes, children) {
  css.class([
    css.margin_top(px(120)),
    css.padding(px(36)),
    css.display("flex"),
    css.justify_content("center"),
  ])
  |> sh.div(attributes, children)
}

fn footer_details(children) {
  css.class([
    css.font_size(rem(0.8)),
    css.line_height("1.4"),
    css.font_weight("500"),
    css.color("var(--text-grey)"),
  ])
  |> sh.div([], children)
}

fn column(children) {
  css.class([
    css.display("flex"),
    css.flex_direction("column"),
    css.align_items("center"),
  ])
  |> sh.div([], children)
}

fn link(href, content) {
  css.class([css.color("#ffaff3")])
  |> sh.a([a.href(href)], [h.text(content)])
}
