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
  Translate(Size, Size)
  TranslateX(Size)
  TranslateY(Size)
  Scale(Float, Float)
  ScaleX(Float)
  ScaleY(Float)
  Rotate(Angle)
  SkewX(Angle)
  SkewY(Angle)
}

fn transform_function_to_string(value: TransformFunction) {
  case value {
    Translate(x, y) ->
      "translate("
      <> string.join([size.to_string(x), size.to_string(y)], ",")
      <> ")"
    TranslateX(x) -> "translateX(" <> size.to_string(x) <> ")"
    TranslateY(y) -> "translateY(" <> size.to_string(y) <> ")"
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

pub fn translate2(x: Size, y: Size) {
  Translate(x, y)
}

/// translate(x) is translate2(x, size.percent(0))
pub fn translate(x: Size) {
  translate2(x, size.percent(0))
}

pub fn translate_x(x: Size) {
  TranslateX(x)
}

pub fn translate_y(y: Size) {
  TranslateY(y)
}

pub fn scale2(x: Float, y: Float) {
  Scale(x, y)
}

/// scale(x) is scale2(x, x)
pub fn scale(x: Float) {
  scale2(x, x)
}

pub fn scale_x(x: Float) {
  ScaleX(x)
}

pub fn scale_y(y: Float) {
  ScaleY(y)
}

pub fn rotate(value: Angle) {
  Rotate(value)
}

pub fn skew_x(x: Angle) {
  SkewX(x)
}

pub fn skew_y(x: Angle) {
  SkewY(x)
}

pub fn none() {
  None
}

pub fn list(values: List(TransformFunction)) {
  TransformList(values)
}

pub fn to_string(value: Transform) {
  let content = case value {
    None -> "none"
    TransformList(transform_list) ->
      list.map(transform_list, transform_function_to_string) |> string.join(" ")
  }

  "transform: " <> content
}
