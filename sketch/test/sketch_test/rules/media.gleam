import sketch/css/length
import sketch/css/media
import startest.{describe, it}
import startest/expect

pub fn sketch_tests() {
  describe("Sketch", [
    describe("CSS at-rules", [
      describe("media", [
        it("it should handle max-width", fn() {
          media.max_width(length.px(10))
          |> expect_media("@media (max-width: 10.0px)")
        }),
        it("it should handle min-width", fn() {
          media.min_width(length.px(10))
          |> expect_media("@media (min-width: 10.0px)")
        }),
        it("it should handle max-height", fn() {
          media.max_height(length.px(10))
          |> expect_media("@media (max-height: 10.0px)")
        }),
        it("it should handle min-height", fn() {
          media.min_height(length.px(10))
          |> expect_media("@media (min-height: 10.0px)")
        }),
        it("it should handle dark_theme", fn() {
          media.dark_theme()
          |> expect_media("@media (prefers-color-scheme: dark)")
        }),
        it("it should handle light_theme", fn() {
          media.light_theme()
          |> expect_media("@media (prefers-color-scheme: light)")
        }),
        it("it should handle and", fn() {
          media.min_width(length.px(1))
          |> media.and(media.min_width(length.px(1)))
          |> expect_media("@media (min-width: 1.0px) and (min-width: 1.0px)")
        }),
        it("it should handle or", fn() {
          media.min_width(length.px(1))
          |> media.or(media.min_width(length.px(1)))
          |> expect_media("@media (min-width: 1.0px) or (min-width: 1.0px)")
        }),
        it("it should handle not", fn() {
          media.not(media.min_width(length.px(1)))
          |> expect_media("@media not (min-width: 1.0px)")
        }),
        it("it should handle landscape", fn() {
          media.landscape()
          |> expect_media("@media (orientation: landscape)")
        }),
        it("it should handle portrait", fn() {
          media.portrait()
          |> expect_media("@media (orientation: portrait)")
        }),
        it("it should handle screen", fn() {
          media.screen()
          |> expect_media("@media screen")
        }),
        it("it should handle print", fn() {
          media.print()
          |> expect_media("@media print")
        }),
        it("it should handle all", fn() {
          media.all()
          |> expect_media("@media all")
        }),
        it("it should handle only", fn() {
          media.only(media.all())
          |> expect_media("@media only all")
        }),
      ]),
    ]),
  ])
}

fn expect_media(media: media.Query, result: String) {
  media
  |> media.to_string
  |> expect.to_equal(result)
}
