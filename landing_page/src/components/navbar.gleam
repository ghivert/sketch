import icons
import redraw
import redraw/dom/attribute as a
import redraw/dom/html as h
import sketch/css
import sketch/css/size.{px}
import sketch/redraw/html as sh

pub fn navbar() {
  use <- redraw.component__("Navbar")
  nav([a.id("navbar")], [
    // icons.small(icons.home()),
    h.div([], []),
    sh.div(css.class([css.display("flex"), css.gap(px(24))]), [], [
      external_icon("https://hexdocs.pm/sketch", icons.book_open()),
      external_icon("https://github.com/ghivert/sketch", icons.github()),
    ]),
  ])
}

pub fn nav(attributes, children) {
  css.class([
    css.display("flex"),
    css.justify_content("space-between"),
    css.padding(px(18)),
    css.margin(px(18)),
    css.gap(px(36)),
    css.background("var(--navbar-background)"),
    css.position("sticky"),
    css.border_radius(px(10)),
    css.top(px(18)),
    css.border("1px solid var(--dark-background)"),
    css.backdrop_filter("blur(8px)"),
  ])
  |> sh.nav(attributes, children)
}

fn external_icon(url, icon) {
  css.class([
    css.color("#aaa"),
    css.transition("all .3s"),
    css.hover([css.color("var(--text-color)")]),
  ])
  |> sh.a([a.href(url)], [icons.small(icon)])
}
