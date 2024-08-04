import gleam/string
import gleeunit
import gleeunit/should
import simplifile
import sketch/constants
import sketch/css

pub fn main() {
  gleeunit.main()
}

// gleeunit test functions end in `_test`
pub fn read_test() {
  let assert Ok(cwd) = simplifile.current_directory()
  let src_folder = string.join([cwd, "test"], "/")
  let dst_folder = string.join([cwd, "styles"], "/")
  let interface_folder = string.join([cwd, "src/sketch/styles"], "/")
  css.generate_stylesheets(src_folder, dst_folder, interface_folder)
  |> should.be_ok
  string.join([cwd, "src", "sketch", "styles", "main_css.gleam"], "/")
  |> simplifile.read
  |> should.be_ok
  |> should.equal(constants.content)
  string.join([cwd, "styles", "main_css.css"], "/")
  |> simplifile.read
  |> should.be_ok
  |> should.equal(constants.css)
}
