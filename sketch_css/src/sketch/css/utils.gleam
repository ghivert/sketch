import gleam/result
import gleam/string
import simplifile

pub type Directories {
  Directories(src: String, dst: String, interface: String)
}

pub fn directories(src: String, dst: String, interface: String) {
  use cwd <- result.map(simplifile.current_directory())
  let src = string.join([cwd, src], "/")
  let dst = string.join([cwd, dst], "/")
  let interface = string.join([cwd, interface], "/")
  Directories(src:, dst:, interface:)
}
