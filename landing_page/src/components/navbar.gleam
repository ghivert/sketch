import icons
import redraw
import redraw/attribute as a
import redraw/html as h
import sketch as s
import sketch/redraw/html as sh
import sketch/size.{px}

pub fn navbar() {
  use <- redraw.component__("Navbar")
  nav([a.id("navbar")], [
    // icons.small(icons.home()),
    h.div([], []),
    sh.div(s.class([s.display("flex"), s.gap(px(24))]), [], [
      external_icon("https://hexdocs.pm/sketch", icons.book_open()),
      external_icon("https://github.com/ghivert/sketch", icons.github()),
    ]),
  ])
}

pub fn nav(attributes, children) {
  s.class([
    s.display("flex"),
    s.justify_content("space-between"),
    s.padding(px(18)),
    s.margin(px(18)),
    s.gap(px(36)),
    s.background("rgba(255, 255, 255, 0.9)"),
    s.position("sticky"),
    s.border_radius(px(10)),
    s.top(px(18)),
    s.border("1px solid #eee"),
    s.backdrop_filter("blur(8px)"),
  ])
  |> sh.nav(attributes, children)
}

fn external_icon(url, icon) {
  s.class([
    s.color("#aaa"),
    s.transition("all .3s"),
    s.hover([s.color("#000000")]),
  ])
  |> sh.a([a.href(url)], [icons.small(icon)])
}
