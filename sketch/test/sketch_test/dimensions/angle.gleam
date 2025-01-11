import sketch/css/angle
import startest.{describe, it}
import startest/expect

pub fn sketch_tests() {
  describe("Sketch", [
    describe("CSS dimensions", [
      describe("angle", [
        it("should handle deg", fn() {
          angle.deg(10.0)
          |> expect_angle("10.0deg")
        }),
        it("should handle rad", fn() {
          angle.rad(10.0)
          |> expect_angle("10.0rad")
        }),
        it("should handle grad", fn() {
          angle.grad(10.0)
          |> expect_angle("10.0grad")
        }),
        it("should handle turn", fn() {
          angle.turn(10.0)
          |> expect_angle("10.0turn")
        }),
      ]),
    ]),
  ])
}

fn expect_angle(angle: angle.Angle, result: String) {
  angle
  |> angle.to_string
  |> expect.to_equal(result)
}
