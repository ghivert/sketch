import craft
import craft/media
import craft/options as craft_options
import craft/size.{px}
import gleam/int
import lustre
import lustre/element/html
import lustre/event

pub type Model =
  Int

pub type Msg {
  Increment
  Decrement
}

pub fn main() {
  let assert Ok(render) =
    craft_options.default()
    |> craft_options.node()
    |> craft.setup()

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
  craft.class([
    craft.background("red"),
    craft.display("flex"),
    craft.flex_direction("row"),
    craft.gap(px(12)),
    craft.padding(px(12)),
    craft.hover([craft.background("yellow")]),
    craft.media(media.max_width(px(450)), [
      craft.background("purple"),
      craft.hover([craft.background("white")]),
    ]),
  ])
  |> craft.to_lustre()
}

fn color_class(model: Model) {
  let back = case model % 2 == 0 {
    True -> "blue"
    False -> "green"
  }
  let id = "color-" <> back
  craft.variable(id, [craft.background(back)])
  |> craft.to_lustre()
}

fn view(model: Model) {
  html.div([main_class()], [
    html.button([event.on_click(Decrement)], [html.text("Decrement")]),
    html.div([color_class(model)], [html.text(int.to_string(model))]),
    html.button([event.on_click(Increment)], [html.text("Increment")]),
  ])
}
