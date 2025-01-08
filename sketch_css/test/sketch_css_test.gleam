import gleam/string
import gleeunit
import gleeunit/should
import simplifile
import sketch/constants
import sketch/css/fs
import sketch/css/generate
import sketch/css/path
import sketch/css/utils

pub fn main() {
  gleeunit.main()
}

// gleeunit test functions end in `_test`
pub fn read_test() {
  let assert Ok(cwd) = fs.cwd()
  let src = path.join(cwd, "test")
  let dst = path.join(cwd, "styles")
  let interface = path.join(cwd, "src/sketch/styles")

  utils.Directories(src:, dst:, interface:)
  |> generate.stylesheets
  |> should.be_ok

  path.join(cwd, "src/sketch/styles/main_css.gleam")
  |> fs.read_file
  |> should.be_ok
  |> should.equal(constants.content)

  path.join(cwd, "styles/main_css.css")
  |> fs.read_file
  |> should.be_ok
  |> should.equal(constants.css)
}
