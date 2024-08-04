import gleam/io
import gleam/string
import gleeunit
import gleeunit/should
import simplifile
import sketch
import sketch/css

pub fn main() {
  gleeunit.main()
}

// gleeunit test functions end in `_test`
pub fn read_test() {
  let assert Ok(cwd) = simplifile.current_directory()
  let src_folder = string.join([cwd, "test"], "/")
  let dst_folder = string.join([cwd, "styles"], "/")
  let interface_folder = string.join([cwd, "test/sketch/styles"], "/")
  css.generate_stylesheets(src_folder, dst_folder, interface_folder)
}
