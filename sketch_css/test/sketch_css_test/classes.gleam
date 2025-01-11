import birdie
import gleam/list
import gleam/result
import gleam/string
import sketch_css/fs
import sketch_css/generate
import sketch_css/path
import sketch_css/utils
import sketch_css_test/helpers
import startest.{describe, it}
import startest/expect

pub fn read_tests() {
  let assert Ok(css_files) = read_test_files()
  let assert Ok(cwd) = fs.cwd()
  let src = path.join(cwd, "test")
  let dst = path.join(cwd, "styles")
  let interface = path.join(cwd, "src/sketch/styles")

  utils.Directories(src:, dst:, interface:)
  |> generate.stylesheets
  |> expect.to_be_ok

  describe("Sketch CSS", [
    describe("generation", {
      use css_file <- list.map(css_files)
      it("should handle " <> css_file, fn() {
        read_snapshot_file(css_file, src, dst, extension: "css")
        read_snapshot_file(css_file, src, interface, extension: "gleam")
      })
    }),
  ])
}

fn read_snapshot_file(
  item: String,
  root: String,
  dst: String,
  extension extension: String,
) -> Nil {
  let assert Ok(name) = string.split(item, on: "/") |> list.last
  item
  |> string.replace(each: root, with: dst)
  |> string.replace(each: "gleam", with: extension)
  |> fs.read_file
  |> expect.to_be_ok
  |> birdie.snap(helpers.multitarget_title(extension <> "_" <> name))
}

fn read_test_files() {
  use cwd <- result.try(fs.cwd())
  let classes =
    string.join([cwd, "test", "sketch_css_test", "classes"], with: "/")
  fs.readdir(classes, recursive: True)
}
