import components
import gleam/int
import redraw
import redraw/dom/attribute as a
import redraw/dom/client
import redraw/dom/events
import redraw/dom/html as h
import sketch/redraw as sr

pub fn main() {
  let app = app()
  let assert Ok(root) = client.create_root("root")
  client.render(root, redraw.strict_mode([sr.provider([app()])]))
}

fn app() {
  let view_counter_description = view_counter_description()
  let view_counter = view_counter()
  let showcase = showcase()
  use <- redraw.component__("App")
  let #(count, set_count) = redraw.use_state_(0)
  let increment = events.on_click(fn(_) { set_count(fn(c) { c + 1 }) })
  let decrement = events.on_click(fn(_) { set_count(fn(c) { c - 1 }) })
  components.body([], [
    components.topbar([], [h.text("Sketch")]),
    components.headline(count, [], [
      components.headline_subtitle([], [h.text("CSS-in-Gleam")]),
      components.headline_emphasize([], [
        h.text("Improve your CSS"),
        h.br([]),
        h.text("with Sketch"),
      ]),
    ]),
    components.counter([], [
      components.counter_body([], [
        components.counter_body_title([], [
          view_counter_description(),
          view_counter(#(count, increment, decrement)),
        ]),
      ]),
      components.card_title([], [h.text("See it in action")]),
    ]),
    showcase(),
  ])
}

fn view_counter_description() {
  use <- redraw.component__("CounterDescription")
  let use_counter = "Use the counter, and see the site changing with the model!"
  let now_edit = "Now, try to edit the code to see the modifications live!"
  h.div([], [
    h.text("Counter"),
    components.counter_subtitle([], [h.text(use_counter)]),
    components.counter_subtitle([], [h.text(now_edit)]),
  ])
}

fn view_counter() {
  use #(count, increment, decrement) <- redraw.component_("Counter")
  let disabled = a.disabled(count <= 0)
  let model = int.to_string(count)
  components.counter_counter([], [
    components.button([decrement, disabled], [h.text("-")]),
    components.value([], [components.value_content([], [h.text(model)])]),
    components.button([increment], [h.text("+")]),
  ])
}

fn showcase() {
  use <- redraw.component__("Showcase")
  components.showcase([], [
    components.showcase_body([], [h.text("Coming soon...")]),
    components.card_title([], [h.text("Showcase")]),
  ])
}
