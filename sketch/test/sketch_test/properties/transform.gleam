import gleam/list
import gleam/string
import sketch/css/angle.{deg}
import sketch/css/length.{px}
import sketch/css/transform

pub fn sketch_test() {
  // describe("Sketch", [
  //   describe("CSS properties", [
  //     describe("transform", [
  //       it("should handle matrix", fn() {
  transform.matrix(1.0, 1.0, 1.0, 1.0, 1.0, 1.0)
  |> expect_transform("matrix(1.0, 1.0, 1.0, 1.0, 1.0, 1.0)")
  // }),
  // it("should handle matrix_3d", fn() {
  transform.matrix_3d({
    let fst = #(1.0, 1.0, 1.0, 1.0)
    let snd = #(2.0, 2.0, 2.0, 2.0)
    let trd = #(3.0, 3.0, 3.0, 3.0)
    let fth = #(4.0, 4.0, 4.0, 4.0)
    #(fst, snd, trd, fth)
  })
  |> expect_transform({
    let fst = list.repeat("1.0", 4)
    let snd = list.repeat("2.0", 4)
    let trd = list.repeat("3.0", 4)
    let fth = list.repeat("4.0", 4)
    [fst, snd, trd, fth]
    |> list.map(string.join(_, with: ", "))
    |> string.join(with: ", ")
    |> string.append(")")
    |> string.append("matrix3d(", _)
  })
  // }),
  // it("should handle translate", fn() {
  transform.translate(length.px(10), length.rem(3.0))
  |> expect_transform("translate(10.0px, 3.0rem)")
  // }),
  // it("should handle translate_3d", fn() {
  let tx = length.px(10)
  let ty = length.rem(3.0)
  let tz = length.cap(1.0)
  transform.translate_3d(tx, ty, tz)
  |> expect_transform("translate3d(10.0px, 3.0rem, 1.0cap)")
  // }),
  // it("should handle translate_x", fn() {
  transform.translate_x(length.px(10))
  |> expect_transform("translateX(10.0px)")
  // }),
  // it("should handle translate_y", fn() {
  transform.translate_y(length.px(10))
  |> expect_transform("translateY(10.0px)")
  // }),
  // it("should handle translate_z", fn() {
  transform.translate_z(length.px(10))
  |> expect_transform("translateZ(10.0px)")
  // }),
  // it("should handle scale", fn() {
  transform.scale(10.0, 10.0)
  |> expect_transform("scale(10.0, 10.0)")
  // }),
  // it("should handle scale_3d", fn() {
  transform.scale_3d(10.0, 10.0, 10.0)
  |> expect_transform("scale3d(10.0, 10.0, 10.0)")
  // }),
  // it("should handle scale_x", fn() {
  transform.scale_x(10.0)
  |> expect_transform("scaleX(10.0)")
  // }),
  // it("should handle scale_y", fn() {
  transform.scale_y(10.0)
  |> expect_transform("scaleY(10.0)")
  // }),
  // it("should handle scale_z", fn() {
  transform.scale_z(10.0)
  |> expect_transform("scaleZ(10.0)")
  // }),
  // it("should handle rotate", fn() {
  transform.rotate(angle.rad(2.0))
  |> expect_transform("rotate(2.0rad)")
  // }),
  // it("should handle rotate_3d", fn() {
  transform.rotate_3d(1.0, 1.0, 1.0, angle.rad(2.0))
  |> expect_transform("rotate3d(1.0, 1.0, 1.0, 2.0rad)")
  // }),
  // it("should handle rotate_x", fn() {
  transform.rotate_x(angle.rad(2.0))
  |> expect_transform("rotateX(2.0rad)")
  // }),
  // it("should handle rotate_y", fn() {
  transform.rotate_y(angle.rad(2.0))
  |> expect_transform("rotateY(2.0rad)")
  // }),
  // it("should handle rotate_z", fn() {
  transform.rotate_z(angle.rad(2.0))
  |> expect_transform("rotateZ(2.0rad)")
  // }),
  // it("should handle skew_x", fn() {
  transform.skew_x(angle.rad(2.0))
  |> expect_transform("skewX(2.0rad)")
  // }),
  // it("should handle skew_y", fn() {
  transform.skew_y(angle.rad(2.0))
  |> expect_transform("skewY(2.0rad)")
  // }),
  // it("should handle perspective", fn() {
  transform.perspective(length.cap(2.0))
  |> expect_transform("perspective(2.0cap)")
  // }),
  // it("should handle no transform", fn() {
  assert transform.to_string([]) == "none"
  // }),
  // it("should handle multiple transforms", fn() {
  assert [transform.rotate(deg(45.0)), transform.translate(px(10), px(10))]
    |> transform.to_string
    == "rotate(45.0deg) translate(10.0px, 10.0px)"
  //       }),
  //     ]),
  //   ]),
  // ])
}

fn expect_transform(transform: transform.Transform, result: String) {
  assert transform.to_string(list.wrap(transform)) == result
}
