import components
import gleam/int
import lustre
import lustre/attribute as a
import lustre/event as e
import sketch
import sketch/lustre as sketch_lustre
import sketch/lustre/element/html as h

pub type Model =
  Int

pub type Msg {
  Increment
  Decrement
}

pub fn app() {
  let assert Ok(cache) = sketch.cache(strategy: sketch.Persistent)
  sketch_lustre.node()
  |> sketch_lustre.compose(view, cache)
  |> lustre.simple(fn(_) { 0 }, update, _)
}

fn update(model: Model, msg: Msg) {
  case msg {
    Increment -> model + 1
    Decrement -> model - 1
  }
}

pub fn view(model: Model) {
  components.body([], [
    components.topbar([], [h.text("Sketch")]),
    components.headline(model, [], [
      components.headline_subtitle([], [h.text("CSS-in-Gleam")]),
      components.headline_emphasize([], [
        h.text("Improve your CSS"),
        h.br_([]),
        h.text("with Sketch"),
      ]),
    ]),
    components.counter([], [
      components.counter_body([], [
        components.counter_body_title([], [
          view_counter_description(),
          view_counter(model),
        ]),
      ]),
      components.card_title([], [h.text("See it in action")]),
    ]),
    components.showcase([], [
      components.showcase_body([], [h.text("Coming soon…")]),
      components.card_title([], [h.text("Showcase")]),
    ]),
  ])
}

fn view_counter_description() {
  let use_counter = "Use the counter, and see the site changing with the model!"
  let now_edit = "Now, try to edit the code to see the modifications live!"
  h.div_([], [
    h.text("Counter"),
    components.counter_subtitle([], [h.text(use_counter)]),
    components.counter_subtitle([], [h.text(now_edit)]),
  ])
}

fn view_counter(model: Model) {
  let disabled = a.disabled(model <= 0)
  let model = int.to_string(model)
  components.counter_counter([], [
    components.button([e.on_click(Decrement), disabled], [h.text("-")]),
    components.value([], [components.value_content([], [h.text(model)])]),
    components.button([e.on_click(Increment)], [h.text("+")]),
  ])
}
