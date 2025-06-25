import sketch/css/angle

pub fn sketch_test() {
  // describe("Sketch", [
  //   describe("CSS dimensions", [
  //     describe("angle", [
  //       it("should handle deg", fn() {
  angle.deg(10.0)
  |> expect_angle("10.0deg")
  // }),
  // it("should handle rad", fn() {
  angle.rad(10.0)
  |> expect_angle("10.0rad")
  // }),
  // it("should handle grad", fn() {
  angle.grad(10.0)
  |> expect_angle("10.0grad")
  // }),
  // it("should handle turn", fn() {
  angle.turn(10.0)
  |> expect_angle("10.0turn")
  //   }),
  // ]),
  // ]),
  // ])
}

fn expect_angle(angle: angle.Angle, result: String) {
  assert angle.to_string(angle) == result
}
