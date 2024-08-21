import redraw
import redraw/handler
import redraw/html as h
import redraw_dom/client
import sketch
import sketch/redraw as sr
import sketch/redraw/html as sh
import sketch/size.{px}

pub fn main() {
  let app = app()
  let root = client.create_root("root")
  client.render(root, redraw.strict_mode([sr.provider([app()])]))
}

fn app() {
  use <- redraw.component__("App")
  let #(count, set_count) = redraw.use_state_(0)
  let on_click = handler.on_click(fn(_) { set_count(fn(c) { c + 1 }) })
  redraw.fragment([
    h.h1([], [h.text("Sketch")]),
    h.div([], [h.text("CSS-in-Gleam, made simple")]),
    h.div([], [h.button([on_click], [h.text("Click me")])]),
    sh.div(section(count), [], [
      h.h2([], [h.text("Sketch CSS")]),
      sh.button(section(count), [], [h.text("Hmmm")]),
    ]),
  ])
}

fn section(count) {
  sketch.class([
    sketch.background(case count % 2 == 0 {
      True -> "red"
      False -> "blue"
    }),
    sketch.color("white"),
    sketch.border_radius(px(8)),
    sketch.transition("all .3s"),
  ])
}
