import gleam/int
import lustre
import lustre/event
import sketch
import sketch/lustre as sketch_lustre
import sketch/lustre/element
import sketch/media
import sketch/size.{px}

pub type Model =
  Int

pub type Msg {
  Increment
  Decrement
}

pub fn main() {
  let init = fn(_) { 0 }

  let assert Ok(cache) = sketch.ephemeral()

  let assert Ok(_) =
    view
    |> sketch_lustre.compose(cache)
    |> lustre.simple(init, update, _)
    |> lustre.start("#app", Nil)
}

fn update(model: Model, msg: Msg) {
  case msg {
    Increment -> model + 1
    Decrement -> model - 1
  }
}

fn main_class(attrs, children) {
  element.element("div", attrs, children, [
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

fn second_class(attrs, children) {
  element.element("div", attrs, children, [
    sketch.background("green"),
    sketch.font_size(px(20)),
    sketch.font_family("-apple-system"),
  ])
}

fn color_class(model: Model, attrs, children) {
  element.element("div", attrs, children, [
    sketch.background(case model % 3 {
      0 -> "blue"
      _ -> "green"
    }),
  ])
}

fn button_class(attrs, children) {
  element.element("button", attrs, children, [
    sketch.cursor("crosshair"),
    sketch.font_size(px(14)),
  ])
}

fn class_test(model: Model) {
  case model % 5 {
    0 -> second_class([], [element.text("Class Test")])
    _ -> element.none()
  }
}

pub fn main_node(attrs, children) {
  element.element("div", attrs, children, [])
}

fn view(model: Model) {
  main_node([], [
    main_class([], [
      button_class([event.on_click(Decrement)], [element.text("Decrement")]),
      color_class(model, [], [element.text(int.to_string(model))]),
      button_class([event.on_click(Increment)], [element.text("Increment")]),
    ]),
    class_test(model),
    class_test(model),
    class_test(model),
  ])
}
