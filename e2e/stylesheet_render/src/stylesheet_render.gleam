import gleam/int
import lustre
import lustre/event
import sketch
import sketch/lustre as sketch_lustre
import sketch/lustre/element
import sketch/lustre/element/html
import sketch/media
import sketch/size.{px}

pub type Model =
  Int

pub type Msg {
  Increment
  Decrement
}

pub fn main() {
  let assert Ok(cache) = sketch.cache(strategy: sketch.Ephemeral)
  let assert Ok(_) =
    sketch_lustre.compose(view, cache)
    |> lustre.simple(fn(_) { 0 }, update, _)
    |> lustre.start("#app", Nil)
}

fn update(model: Model, msg: Msg) {
  case msg {
    Increment -> model + 1
    Decrement -> model - 1
  }
}

fn body(attrs, children) {
  html.main(attrs, children, [
    sketch.background("red"),
    sketch.display("flex"),
    sketch.flex_direction("row"),
    sketch.gap(px(12)),
    sketch.padding(px(12)),
    sketch.hover([sketch.background("yellow")]),
    sketch.media(media.max_width(px(450)), [
      sketch.background("purple"),
      sketch.hover([sketch.background("white")]),
    ]),
  ])
}

fn green_background(attrs, children) {
  html.div(attrs, children, [
    sketch.background("green"),
    sketch.font_size(px(20)),
    sketch.font_family("-apple-system"),
  ])
}

fn colored_background(model: Model, attrs, children) {
  html.div(attrs, children, [
    sketch.background(case model % 3 {
      0 -> "blue"
      _ -> "green"
    }),
  ])
}

fn increment_decrement(attrs, children) {
  html.button(attrs, children, [
    sketch.cursor("crosshair"),
    sketch.font_size(px(14)),
  ])
}

fn class_test(model: Model) {
  case model % 5 {
    0 -> green_background([], [element.text("Class Test")])
    _ -> element.none()
  }
}

fn view(model: Model) {
  html.div_([], [
    body([], [
      increment_decrement([event.on_click(Decrement)], [
        element.text("Decrement"),
      ]),
      colored_background(model, [], [element.text(int.to_string(model))]),
      increment_decrement([event.on_click(Increment)], [
        element.text("Increment"),
      ]),
    ]),
    class_test(model),
    class_test(model),
    class_test(model),
  ])
}
