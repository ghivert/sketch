import sketch/css/font_face
import startest.{describe, it}
import startest/expect

pub fn sketch_tests() {
  describe("Sketch", [
    describe("CSS at-rules", [
      describe("font-face", [
        it("should handle ascent-override", fn() {
          font_face.ascent_override(1.0)
          |> expect_font_face("ascent-override: 1.0%")
        }),
        it("should handle descent-override", fn() {
          font_face.descent_override(1.0)
          |> expect_font_face("descent-override: 1.0%")
        }),
        it("should handle font-display", fn() {
          font_face.font_display("example")
          |> expect_font_face("font-display: example")
        }),
        it("should handle font-family", fn() {
          font_face.font_family("example")
          |> expect_font_face("font-family: example")
        }),
        it("should handle font-stretch", fn() {
          font_face.font_stretch("example")
          |> expect_font_face("font-stretch: example")
        }),
        it("should handle font-style", fn() {
          font_face.font_style("example")
          |> expect_font_face("font-style: example")
        }),
        it("should handle font-weight", fn() {
          font_face.font_weight("example")
          |> expect_font_face("font-weight: example")
        }),
        it("should handle font-feature-settings", fn() {
          font_face.font_feature_settings("example")
          |> expect_font_face("font-feature-settings: example")
        }),
        it("should handle font-variation-settings", fn() {
          font_face.font_variation_settings("example")
          |> expect_font_face("font-variation-settings: example")
        }),
        it("should handle line-gap-override", fn() {
          font_face.line_gap_override(1.0)
          |> expect_font_face("line-gap-override: 1.0%")
        }),
        it("should handle size-adjust", fn() {
          font_face.size_adjust(1.0)
          |> expect_font_face("size-adjust: 1.0%")
        }),
        it("should handle src", fn() {
          font_face.src("example")
          |> expect_font_face("src: example")
        }),
        it("should handle unicode-range", fn() {
          font_face.unicode_range("example")
          |> expect_font_face("unicode-range: example")
        }),
      ]),
    ]),
  ])
}

fn expect_font_face(font_face: font_face.FontFace, result: String) {
  font_face
  |> font_face.to_string
  |> expect.to_equal(result)
}
