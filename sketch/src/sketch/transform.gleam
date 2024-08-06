import gleam/float
import gleam/list
import gleam/option.{type Option}
import gleam/string
import sketch/angle.{type Angle}
import sketch/size.{type Size}

pub opaque type Transform {
  None
  TransformList(List(TransformFunction))
}

pub opaque type TransformFunction {
  Translate(Size, Option(Size))
  TranslateX(Size)
  TranslateY(Size)
  Scale(Float, Option(Float))
  ScaleX(Float)
  ScaleY(Float)
  Rotate(Angle)
  Skew(Angle, Option(Angle))
  SkewX(Angle)
  SkewY(Angle)
}

fn transform_function_to_string(value: TransformFunction) {
  case value {
    Translate(x, option.None) ->
      "translate("
      <> string.join([size.to_string(x), size.to_string(x)], ",")
      <> ")"
    Translate(x, option.Some(y)) ->
      "translate("
      <> string.join([size.to_string(x), size.to_string(y)], ",")
      <> ")"
    TranslateX(x) -> "translateX(" <> size.to_string(x) <> ")"
    TranslateY(y) -> "translateY(" <> size.to_string(y) <> ")"
    Scale(x, option.None) ->
      "scale("
      <> string.join([float.to_string(x), float.to_string(x)], ",")
      <> ")"
    Scale(x, option.Some(y)) ->
      "scale("
      <> string.join([float.to_string(x), float.to_string(y)], ",")
      <> ")"
    ScaleX(x) -> "scaleX(" <> float.to_string(x) <> ")"
    ScaleY(y) -> "scaleY(" <> float.to_string(y) <> ")"
    Rotate(ang) -> "rotate(" <> angle.to_string(ang) <> ")"
    Skew(x, option.None) ->
      "translate(" <> string.join([angle.to_string(x), "0"], ",") <> ")"
    Skew(x, option.Some(y)) ->
      "translate("
      <> string.join([angle.to_string(x), angle.to_string(y)], ",")
      <> ")"
    SkewX(x) -> "skewX(" <> angle.to_string(x) <> ")"
    SkewY(y) -> "skewY(" <> angle.to_string(y) <> ")"
  }
}

pub fn translate(x: Size, y: Option(Size)) {
  Translate(x, y)
}

pub fn translate_x(x: Size) {
  TranslateX(x)
}

pub fn translate_y(y: Size) {
  TranslateY(y)
}

pub fn scale(x: Float, y: Option(Float)) {
  Scale(x, y)
}

pub fn scale_x(x: Float) {
  ScaleX(x)
}

pub fn scale_y(y: Float) {
  ScaleY(y)
}

pub fn skew(x: Angle, y: Option(Angle)) {
  Skew(x, y)
}

pub fn skew_x(x: Angle) {
  SkewX(x)
}

pub fn skew_y(x: Angle) {
  SkewY(x)
}

pub fn to_string(value: Transform) {
  let content = case value {
    None -> "none"
    TransformList(transform_list) ->
      list.map(transform_list, transform_function_to_string) |> string.join(" ")
  }

  "transform: " <> content
}
