import birdie
import gleam/string
import gleeunit
import gleeunit/should
import sketch
import sketch/css
import sketch/css/media
import sketch/css/size

pub fn main() {
  gleeunit.main()
}

pub fn css_test() {
  let assert Ok(stylesheet) = sketch.stylesheet(strategy: sketch.Ephemeral)
  let #(stylesheet, class_name) = sketch.class_name(dummy_class(), stylesheet)
  let content = sketch.render(stylesheet)
  content |> string.contains(class_name) |> should.be_true
  birdie.snap(title: "css_test", content:)
}

fn dummy_class() {
  css.class([
    css.background("red"),
    css.hover([
      css.background("blue"),
      css.child(button_class(), [
        css.background("red"),
        css.font_family("Verdana"),
      ]),
    ]),
    css.child(button_class(), [
      css.background("blue") |> css.important,
      css.font_size(size.rem(2.0)),
    ]),
    css.media(media.max_width(size.px(700)), [css.background("yellow")]),
  ])
}

fn button_class() {
  css.class([
    css.appearance("none"),
    css.background("none"),
    css.border("1px solid black"),
    css.font_family("inherit"),
    css.font_size_("inherit"),
  ])
}
