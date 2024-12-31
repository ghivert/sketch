import gleeunit/should
import sketch/css/angle
import sketch/css/size
import sketch/css/transform

pub fn translate_test() {
  [transform.translate(size.px(10))]
  |> transform.to_string
  |> should.equal("translate(10.0px,0.0%)")
}

pub fn translate2_test() {
  [transform.translate2(size.px(10), size.rem(3.0))]
  |> transform.to_string
  |> should.equal("translate(10.0px,3.0rem)")
}

pub fn translate_x() {
  [transform.translate_x(size.px(10))]
  |> transform.to_string
  |> should.equal("translateX(10px)")
}

pub fn translate_y() {
  [transform.translate_y(size.px(10))]
  |> transform.to_string
  |> should.equal("translateY(10px)")
}

pub fn scale2_test() {
  [transform.scale2(10.0, 10.0)]
  |> transform.to_string
  |> should.equal("scale(10.0,10.0)")
}

pub fn scale_test() {
  [transform.scale(10.0)]
  |> transform.to_string
  |> should.equal("scale(10.0,10.0)")
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

pub fn rotate_test() {
  [transform.rotate(angle.rad(2.0))]
  |> transform.to_string
  |> should.equal("rotate(2.0rad)")
}

pub fn skew_x() {
  [transform.skew_x(angle.rad(2.0))]
  |> transform.to_string
  |> should.equal("skewX(2.0rad)")
}

pub fn skew_y() {
  [transform.skew_y(angle.rad(2.0))]
  |> transform.to_string
  |> should.equal("skewY(2.0rad)")
}

pub fn translate_equiv_test() {
  let current = [transform.translate(size.px(10))] |> transform.to_string
  let expected =
    [transform.translate2(size.px(10), size.percent(0))]
    |> transform.to_string

  should.equal(current, expected)
}

pub fn scale_equiv_test() {
  let current = [transform.scale(10.0)] |> transform.to_string
  let expected = [transform.scale2(10.0, 10.0)] |> transform.to_string

  should.equal(current, expected)
}

pub fn transform_none_test() {
  transform.to_string([])
  |> should.equal("none")
}
