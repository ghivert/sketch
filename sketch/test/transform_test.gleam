import gleeunit/should
import sketch/css/angle
import sketch/css/length
import sketch/css/transform

pub fn matrix_test() {
  [transform.matrix(1.0, 1.0, 1.0, 1.0, 1.0, 1.0)]
  |> transform.to_string
  |> should.equal("matrix(1.0, 1.0, 1.0, 1.0, 1.0, 1.0)")
}

pub fn matrix_3d_test() {
  [
    transform.matrix_3d(
      #(#(1.0, 1.0, 1.0, 1.0), #(2.0, 2.0, 2.0, 2.0), #(3.0, 3.0, 3.0, 3.0), {
        #(4.0, 4.0, 4.0, 4.0)
      }),
    ),
  ]
  |> transform.to_string
  |> should.equal(
    "matrix3d(1.0, 1.0, 1.0, 1.0, 2.0, 2.0, 2.0, 2.0, 3.0, 3.0, 3.0, 3.0, 4.0, 4.0, 4.0, 4.0)",
  )
}

pub fn translate_test() {
  [transform.translate(length.px(10), length.rem(3.0))]
  |> transform.to_string
  |> should.equal("translate(10.0px, 3.0rem)")
}

pub fn translate_3d_test() {
  [transform.translate_3d(length.px(10), length.rem(3.0), length.cap(1.0))]
  |> transform.to_string
  |> should.equal("translate3d(10.0px, 3.0rem, 1.0cap)")
}

pub fn translate_x() {
  [transform.translate_x(length.px(10))]
  |> transform.to_string
  |> should.equal("translateX(10px)")
}

pub fn translate_y() {
  [transform.translate_y(length.px(10))]
  |> transform.to_string
  |> should.equal("translateY(10px)")
}

pub fn translate_z() {
  [transform.translate_z(length.px(10))]
  |> transform.to_string
  |> should.equal("translateZ(10px)")
}

pub fn scale_test() {
  [transform.scale(10.0, 10.0)]
  |> transform.to_string
  |> should.equal("scale(10.0, 10.0)")
}

pub fn scale_3d_test() {
  [transform.scale_3d(10.0, 10.0, 10.0)]
  |> transform.to_string
  |> should.equal("scale3d(10.0, 10.0, 10.0)")
}

pub fn scale_x_test() {
  [transform.scale_x(10.0)]
  |> transform.to_string
  |> should.equal("scaleX(10.0)")
}

pub fn scale_y_test() {
  [transform.scale_y(10.0)]
  |> transform.to_string
  |> should.equal("scaleY(10.0)")
}

pub fn scale_z_test() {
  [transform.scale_z(10.0)]
  |> transform.to_string
  |> should.equal("scaleZ(10.0)")
}

pub fn rotate_test() {
  [transform.rotate(angle.rad(2.0))]
  |> transform.to_string
  |> should.equal("rotate(2.0rad)")
}

pub fn rotate_3d_test() {
  [transform.rotate_3d(1.0, 1.0, 1.0, angle.rad(2.0))]
  |> transform.to_string
  |> should.equal("rotate3d(1.0, 1.0, 1.0, 2.0rad)")
}

pub fn rotate_x_test() {
  [transform.rotate_x(angle.rad(2.0))]
  |> transform.to_string
  |> should.equal("rotateX(2.0rad)")
}

pub fn rotate_y_test() {
  [transform.rotate_y(angle.rad(2.0))]
  |> transform.to_string
  |> should.equal("rotateY(2.0rad)")
}

pub fn rotate_z_test() {
  [transform.rotate_z(angle.rad(2.0))]
  |> transform.to_string
  |> should.equal("rotateZ(2.0rad)")
}

pub fn skew_x_test() {
  [transform.skew_x(angle.rad(2.0))]
  |> transform.to_string
  |> should.equal("skewX(2.0rad)")
}

pub fn skew_y_test() {
  [transform.skew_y(angle.rad(2.0))]
  |> transform.to_string
  |> should.equal("skewY(2.0rad)")
}

pub fn perspective() {
  [transform.perspective(length.cap(2.0))]
  |> transform.to_string
  |> should.equal("perspective(2.0cap)")
}

pub fn transform_none_test() {
  transform.to_string([])
  |> should.equal("none")
}
