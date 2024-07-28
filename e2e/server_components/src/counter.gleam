import gleam/int
import lustre
import lustre/event
import sketch
import sketch/lustre as sketch_lustre
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
  let assert Ok(cache) = sketch.cache(strategy: sketch.Persistent)
  let assert Ok(_) =
    sketch_lustre.compose(view, cache)
    |> lustre.simple(fn(_) { 0 }, update, _)
    |> lustre.start("#app", Nil)
}

pub fn app() {
  let assert Ok(cache) = sketch.cache(strategy: sketch.Persistent)
  sketch_lustre.compose(view, cache)
  |> lustre.simple(fn(_) { 0 }, update, _)
}

fn update(model: Model, msg: Msg) {
  case msg {
    Increment -> model + 1
    Decrement -> model - 1
  }
}

fn main_class(attrs, children) {
  html.div(attrs, children, [
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

fn color_class(model: Model, attrs, children) {
  html.div(attrs, children, [
    sketch.background(case model % 3 {
      0 -> "blue"
      _ -> "green"
    }),
  ])
}

fn button_class(attrs, children) {
  html.button(attrs, children, [
    sketch.cursor("crosshair"),
    sketch.font_size_("14px"),
  ])
}

fn view(model: Model) {
  main_class([], [
    button_class([event.on_click(Decrement)], [html.text("Decrement")]),
    color_class(model, [], [html.text(int.to_string(model))]),
    button_class([event.on_click(Increment)], [html.text("Increment")]),
  ])
}
