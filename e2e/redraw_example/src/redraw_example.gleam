import redraw
import redraw/handler
import redraw/html as h
import redraw_dom/client
import sketch as s
import sketch/redraw as sr
import sketch/redraw/html as sh
import sketch/size.{px, vh}

pub fn main() {
  let app = app()
  let root = client.create_root("root")
  client.render(root, redraw.strict_mode([sr.provider([app()])]))
}

fn app() {
  use <- redraw.component__("App")
  let #(count, set_count) = redraw.use_state_(0)
  let on_click = handler.on_click(fn(_) { set_count(fn(c) { c + 1 }) })
  sh.div(body(), [], [
    sh.div(wrapper(), [], [
      sh.h1(main_title(), [], [h.text("Sketch")]),
      sh.div(subtitle(), [], [h.text("CSS-in-Gleam, made simple")]),
    ]),
    sh.div(button_wrapper(), [], [
      sh.button(button(), [on_click], [h.text("Change color")]),
      sh.div(square_wrapper(), [], [
        sh.div(square_behind(count + 1), [], []),
        sh.div(square(count), [], []),
      ]),
    ]),
  ])
}

fn body() {
  s.class([
    s.height(vh(100)),
    s.display("flex"),
    s.flex_direction("column"),
    s.justify_content("center"),
    s.align_items("center"),
    s.gap(px(24)),
  ])
}

fn wrapper() {
  s.class([s.display("flex"), s.gap(px(12)), s.align_items("baseline")])
}

fn main_title() {
  s.class([s.margin(px(0))])
}

fn subtitle() {
  s.class([])
}

fn button_wrapper() {
  s.class([s.display("flex"), s.gap(px(40)), s.align_items("center")])
}

fn square_wrapper() {
  s.class([s.position("relative")])
}

fn square_base(count) {
  s.class([
    s.background(compute_bg(count)),
    s.width(px(80)),
    s.height(px(80)),
    s.border_radius(px(10)),
    s.transition("all .3s"),
  ])
}

fn square(count) {
  s.class([s.compose(square_base(count)), s.position("relative"), s.z_index(1)])
}

fn square_behind(count) {
  s.class([
    s.compose(square_base(count)),
    s.position("absolute"),
    s.bottom(px(-40)),
    s.right(px(-40)),
    s.z_index(0),
  ])
}

fn compute_bg(count) {
  case count % 2 == 0 {
    True -> "red"
    False -> "blue"
  }
}

fn button() {
  s.class([
    s.margin_top(px(40)),
    s.background("#ddd"),
    s.color("black"),
    s.border_radius(px(8)),
    s.transition("all .3s"),
    s.border("none"),
    s.appearance("none"),
    s.font_family("inherit"),
    s.font_size(px(16)),
    s.padding(px(12)),
    s.font_weight("bold"),
    s.cursor("pointer"),
  ])
}
