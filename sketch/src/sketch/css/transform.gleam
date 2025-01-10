//// `transform` helps you write CSS transforms function, in a fully type-safe
//// way. Every transform functions are implemented.
////
//// ---
////
//// [MDN Documentation](https://developer.mozilla.org/docs/Web/CSS/transform-function)

import gleam/bool
import gleam/float
import gleam/list
import gleam/string
import sketch/css/angle.{type Angle}
import sketch/css/length.{type Length}

/// The `<transform-function>` CSS data type represents a transformation that
/// affects an element's appearance. Transformation functions can rotate, resize,
/// distort, or move an element in 2D or 3D space.
/// It is used in the transform property.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/CSS/transform-function)
pub opaque type Transform {
  Matrix(Float, Float, Float, Float, Float, Float)
  Matrix3D(
    Float,
    Float,
    Float,
    Float,
    Float,
    Float,
    Float,
    Float,
    Float,
    Float,
    Float,
    Float,
    Float,
    Float,
    Float,
    Float,
  )
  Translate(Length, Length)
  Translate3D(Length, Length, Length)
  TranslateX(Length)
  TranslateY(Length)
  TranslateZ(Length)
  Scale(Float, Float)
  Scale3D(Float, Float, Float)
  ScaleX(Float)
  ScaleY(Float)
  ScaleZ(Float)
  Rotate(Angle)
  Rotate3D(Float, Float, Float, Angle)
  RotateX(Angle)
  RotateY(Angle)
  RotateZ(Angle)
  Skew(Angle, Angle)
  SkewX(Angle)
  SkewY(Angle)
  Perspective(Length)
}

/// The `matrix()` CSS function defines a homogeneous 2D transformation matrix.
///
/// > ```gleam
/// > transform.matrix(a, b, c, d, tx, ty)
/// > ```
/// > is a shorthand for
/// > ```gleam
/// > transform.matrix_3d(#(
/// >    #(a,  b,  0,  0),
/// >    #(c,  d,  0,  0),
/// >    #(0,  0,  1,  0),
/// >    #(tx, ty, 0,  1)
/// > ))
/// > ```
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/CSS/transform-function/matrix)
pub fn matrix(
  a: Float,
  b: Float,
  c: Float,
  d: Float,
  tx: Float,
  ty: Float,
) -> Transform {
  Matrix(a, b, c, d, tx, ty)
}

/// The `matrix3d()` CSS function defines a 3D transformation as a 4x4
/// homogeneous matrix.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/CSS/transform-function/matrix3d)
pub fn matrix_3d(
  values: #(
    #(Float, Float, Float, Float),
    #(Float, Float, Float, Float),
    #(Float, Float, Float, Float),
    #(Float, Float, Float, Float),
  ),
) -> Transform {
  let #(
    #(a1, b1, c1, d1),
    #(a2, b2, c2, d2),
    #(a3, b3, c3, d3),
    #(a4, b4, c4, d4),
  ) = values
  Matrix3D(a1, b1, c1, d1, a2, b2, c2, d2, a3, b3, c3, d3, a4, b4, c4, d4)
}

/// The `translate()` CSS function repositions an element in the horizontal
/// and/or vertical directions.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/CSS/transform-function/translate)
pub fn translate(x: Length, y: Length) -> Transform {
  Translate(x, y)
}

/// The `translate3d()` CSS function repositions an element in 3D space.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/CSS/transform-function/translate3d)
pub fn translate_3d(x: Length, y: Length, z: Length) -> Transform {
  Translate3D(x, y, z)
}

/// The `translateX()` CSS function repositions an element horizontally on the
/// 2D plane.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/CSS/transform-function/translateX)
pub fn translate_x(x: Length) -> Transform {
  TranslateX(x)
}

/// The `translateY()` CSS function repositions an element vertically on the
/// 2D plane.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/CSS/transform-function/translateY)
pub fn translate_y(y: Length) -> Transform {
  TranslateY(y)
}

/// The `translateZ()` CSS function repositions an element along the z-axis in
/// 3D space, i.e., closer to or farther away from the viewer.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/CSS/transform-function/translateZ)
pub fn translate_z(y: Length) -> Transform {
  TranslateZ(y)
}

/// The `scale()` CSS function defines a transformation that resizes an element
/// on the 2D plane. Because the amount of scaling is defined by a vector [sx, sy],
/// it can resize the horizontal and vertical dimensions at different scales.
///
/// > The `scale()` function only scales in 2D. To scale in 3D, use `scale3d()` instead.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/CSS/transform-function/scale)
pub fn scale(x: Float, y: Float) -> Transform {
  Scale(x, y)
}

/// The `scale3d()` CSS function defines a transformation that resizes an element
/// in 3D space. Because the amount of scaling is defined by a vector [sx, sy, sz],
/// it can resize different dimensions at different scales.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/CSS/transform-function/scale3d)
pub fn scale_3d(x: Float, y: Float, z: Float) -> Transform {
  Scale3D(x, y, z)
}

/// The `scaleX()` CSS function defines a transformation that resizes an element
/// along the x-axis (horizontally).
///
/// > `scaleX(sx)` is equivalent to `scale(sx, 1)` or `scale3d(sx, 1, 1)`.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/CSS/transform-function/scaleX)
pub fn scale_x(x: Float) -> Transform {
  ScaleX(x)
}

/// The `scaleY()` CSS function defines a transformation that resizes an element
/// along the y-axis (vertically).
///
/// > `scaleY(sy)` is equivalent to `scale(1, sy)` or `scale3d(1, sy, 1)`.
/// >
/// > `transform: rotateX(180deg)`; === `transform: scaleY(-1);`
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/CSS/transform-function/scaleY)
pub fn scale_y(y: Float) -> Transform {
  ScaleY(y)
}

/// The `scaleZ()` CSS function defines a transformation that resizes an element
/// along the z-axis.
///
/// > `scaleZ(sy)` is equivalent to `scale3d(1, 1, sy)`.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/CSS/transform-function/scaleZ)
pub fn scale_z(y: Float) -> Transform {
  ScaleZ(y)
}

/// The `rotate()` CSS function defines a transformation that rotates an element
/// around a fixed point on the 2D plane, without deforming it.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/CSS/transform-function/rotate)
pub fn rotate(value: Angle) -> Transform {
  Rotate(value)
}

/// The `rotate3d()` CSS function defines a transformation that rotates an element
/// around a fixed axis in 3D space, without deforming it
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/CSS/transform-function/rotate3d)
pub fn rotate_3d(x: Float, y: Float, z: Float, value: Angle) -> Transform {
  Rotate3D(x, y, z, value)
}

/// The `rotateX()` CSS function defines a transformation that rotates an element
/// oarend the x-axis (horizontal) without deforming it.
///
/// > `rotateX(a)` is equivalent to `rotate3d(1, 0, 0, a)`.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/CSS/transform-function/rotateX)
pub fn rotate_x(x: Angle) -> Transform {
  RotateX(x)
}

/// The `rotateY()` CSS function defines a transformation that rotates an element
/// oarend the y-axis (vertical) without deforming it.
///
/// > `rotateY(a)` is equivalent to `rotate3d(0, 1, 0, a)`.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/CSS/transform-function/rotateY)
pub fn rotate_y(y: Angle) -> Transform {
  RotateY(y)
}

/// The `rotateZ()` CSS function defines a transformation that rotates an element
/// oarend the z-axis without deforming it.
///
/// > `rotateZ(a)` is equivalent to `rotate3d(0, 0, 1, a)`.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/CSS/transform-function/rotateZ)
pub fn rotate_z(y: Angle) -> Transform {
  RotateZ(y)
}

/// The `skew()` CSS function defines a transformation that skews an element on
/// the 2D plane.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/CSS/transform-function/skew)
pub fn skew(x: Angle, y: Angle) -> Transform {
  Skew(x, y)
}

/// The `skewX()` CSS function defines a transformation that skews an element
/// in the horizontal direction on the 2D plane.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/CSS/transform-function/skewX)
pub fn skew_x(x: Angle) -> Transform {
  SkewX(x)
}

/// The `skewY()` CSS function defines a transformation that skews an element
/// in the vertical direction on the 2D plane.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/CSS/transform-function/skewY)
pub fn skew_y(x: Angle) -> Transform {
  SkewY(x)
}

/// The `perspective()` CSS function defines a transformation that sets the
/// distance between the user and the z=0 plane, the perspective from which the
/// viewer would be if the 2-dimensional interface were 3-dimensional.
///
/// ---
///
/// [MDN Documentation](https://developer.mozilla.org/docs/Web/CSS/transform-function/perspective)
pub fn perspective(x: Length) -> Transform {
  Perspective(x)
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
    Matrix(a, b, c, d, tx, ty) -> {
      [a, b, c, d, tx, ty]
      |> list.map(float.to_string)
      |> string.join(with: ", ")
      |> fn(matrix) { "matrix(" <> matrix <> ")" }
    }
    Matrix3D(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p) -> {
      [a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p]
      |> list.map(float.to_string)
      |> string.join(with: ", ")
      |> fn(matrix) { "matrix3d(" <> matrix <> ")" }
    }
    Translate(x, y) -> {
      let x = length.to_string(x)
      let y = length.to_string(y)
      "translate(" <> string.join([x, y], ", ") <> ")"
    }
    Translate3D(x, y, z) -> {
      let x = length.to_string(x)
      let y = length.to_string(y)
      let z = length.to_string(z)
      "translate3d(" <> string.join([x, y, z], ", ") <> ")"
    }
    TranslateX(x) -> "translateX(" <> length.to_string(x) <> ")"
    TranslateY(y) -> "translateY(" <> length.to_string(y) <> ")"
    TranslateZ(z) -> "translateZ(" <> length.to_string(z) <> ")"
    Scale(x, y) -> {
      let x = float.to_string(x)
      let y = float.to_string(y)
      "scale(" <> string.join([x, y], ", ") <> ")"
    }
    Scale3D(x, y, z) -> {
      let x = float.to_string(x)
      let y = float.to_string(y)
      let z = float.to_string(z)
      "scale3d(" <> string.join([x, y, z], ", ") <> ")"
    }
    ScaleX(x) -> "scaleX(" <> float.to_string(x) <> ")"
    ScaleY(y) -> "scaleY(" <> float.to_string(y) <> ")"
    ScaleZ(z) -> "scaleZ(" <> float.to_string(z) <> ")"
    Rotate(ang) -> "rotate(" <> angle.to_string(ang) <> ")"
    Rotate3D(x, y, z, ang) -> {
      let x = float.to_string(x)
      let y = float.to_string(y)
      let z = float.to_string(z)
      let angle = angle.to_string(ang)
      "rotate3d(" <> string.join([x, y, z, angle], ", ") <> ")"
    }
    RotateX(x) -> "rotateX(" <> angle.to_string(x) <> ")"
    RotateY(y) -> "rotateY(" <> angle.to_string(y) <> ")"
    RotateZ(z) -> "rotateZ(" <> angle.to_string(z) <> ")"
    Skew(x, y) -> {
      let x = angle.to_string(x)
      let y = angle.to_string(y)
      "skew(" <> string.join([x, y], ", ") <> ")"
    }
    SkewX(x) -> "skewX(" <> angle.to_string(x) <> ")"
    SkewY(y) -> "skewY(" <> angle.to_string(y) <> ")"
    Perspective(x) -> "perspective(" <> length.to_string(x) <> ")"
  }
}
