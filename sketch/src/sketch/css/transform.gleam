import gleam/bool
import gleam/float
import gleam/list
import gleam/string
import sketch/css/angle.{type Angle}
import sketch/css/length.{type Length}

pub opaque type Transform {
  Translate(Length, Length)
  TranslateX(Length)
  TranslateY(Length)
  Scale(Float, Float)
  ScaleX(Float)
  ScaleY(Float)
  Rotate(Angle)
  SkewX(Angle)
  SkewY(Angle)
}

pub fn translate2(x: Length, y: Length) -> Transform {
  Translate(x, y)
}

/// `translate(x)` is `translate2(x, length.percent(0))`
pub fn translate(x: Length) -> Transform {
  translate2(x, length.percent(0))
}

pub fn translate_x(x: Length) -> Transform {
  TranslateX(x)
}

pub fn translate_y(y: Length) -> Transform {
  TranslateY(y)
}

pub fn scale2(x: Float, y: Float) -> Transform {
  Scale(x, y)
}

/// `scale(x)` is `scale2(x, x)`
pub fn scale(x: Float) -> Transform {
  scale2(x, x)
}

pub fn scale_x(x: Float) -> Transform {
  ScaleX(x)
}

pub fn scale_y(y: Float) -> Transform {
  ScaleY(y)
}

pub fn rotate(value: Angle) -> Transform {
  Rotate(value)
}

pub fn skew_x(x: Angle) -> Transform {
  SkewX(x)
}

pub fn skew_y(x: Angle) -> Transform {
  SkewY(x)
}

/// Internal function, can be used if you need to go from a transform function
/// to a String in case you're building on top of sketch.
@internal
pub fn to_string(value: List(Transform)) -> String {
  use <- bool.guard(when: list.is_empty(value), return: "none")
  list.map(value, transform_to_string)
  |> string.join(" ")
}

fn transform_to_string(value: Transform) -> String {
  case value {
    Translate(x, y) ->
      "translate("
      <> string.join([length.to_string(x), length.to_string(y)], ",")
      <> ")"
    TranslateX(x) -> "translateX(" <> length.to_string(x) <> ")"
    TranslateY(y) -> "translateY(" <> length.to_string(y) <> ")"
    Scale(x, y) ->
      "scale("
      <> string.join([float.to_string(x), float.to_string(y)], ",")
      <> ")"
    ScaleX(x) -> "scaleX(" <> float.to_string(x) <> ")"
    ScaleY(y) -> "scaleY(" <> float.to_string(y) <> ")"
    Rotate(ang) -> "rotate(" <> angle.to_string(ang) <> ")"
    SkewX(x) -> "skewX(" <> angle.to_string(x) <> ")"
    SkewY(y) -> "skewY(" <> angle.to_string(y) <> ")"
  }
}
