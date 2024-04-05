import gleam/int
import lustre
import lustre/element/html
import lustre/event
import sketch
import sketch/media
import sketch/options as sketch_options
import sketch/size.{px}

pub type Model =
  Int

pub type Msg {
  Increment
  Decrement
}

pub fn main() {
  let assert Ok(render) =
    sketch_options.node()
    |> sketch.lustre_setup()

  let assert Ok(_) =
    fn(_) { 0 }
    |> lustre.simple(update, render(view))
    |> lustre.start("#app", Nil)
}

fn update(model: Model, msg: Msg) {
  case msg {
    Increment -> model + 1
    Decrement -> model - 1
  }
}

fn main_class() {
  sketch.class([
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
  |> sketch.to_lustre()
}

fn color_class(model: Model) {
  let back = case model % 3 {
    0 -> "blue"
    _ -> "green"
  }
  let id = "color-" <> back
  sketch.dynamic(id, [sketch.background(back)])
  |> sketch.to_lustre()
}

fn view(model: Model) {
  html.div([main_class()], [
    html.button([event.on_click(Decrement)], [html.text("Decrement")]),
    html.div([color_class(model)], [html.text(int.to_string(model))]),
    html.button([event.on_click(Increment)], [html.text("Increment")]),
  ])
}
