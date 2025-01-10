import gleam/bool
import gleam/list
import gleam/option.{type Option}
import gleam/pair
import gleam/result
import gleam/string
import sketch_css/fs
import snag
import tom
import uniconfig

/// Directories used in scripts.
/// - `src` indicates where to search for styles files.
/// - `dst` indicates where to output the stylesheets.
/// - `interface` indicates where to output the Gleam interface files.
pub type Directories {
  Directories(src: String, dst: String, interface: String)
}

pub fn directories(
  src: Option(String),
  dst: Option(String),
  interface: Option(String),
) -> snag.Result(Directories) {
  let config = uniconfig.read("sketch_css") |> option.from_result
  use cwd <- result.map(fs.cwd())
  let src = read_directory("src", src, cwd, config, "src")
  let dst = read_directory("dst", dst, cwd, config, "public/styles")
  Directories(src:, dst:, interface: {
    read_directory("interface", interface, cwd, config, "src/sketch/styles")
  })
}

fn read_directory(
  key: String,
  flag: Option(String),
  cwd: String,
  config: Option(uniconfig.Config),
  default: String,
) -> String {
  flag
  |> option.map(pair.new(cwd, _))
  |> option.lazy_unwrap(fn() {
    option.then(config, read_config_directory(_, key))
    |> option.unwrap(#(cwd, default))
  })
  |> fn(res) { string.join([res.0, res.1], with: "/") }
}

fn read_config_directory(
  config: uniconfig.Config,
  key: String,
) -> Option(#(String, String)) {
  config.config
  |> tom.get_string([key])
  |> option.from_result
  |> option.map(pair.new(config.directory, _))
}

pub fn remove_last_segment(path: String) {
  let segments = string.split(path, on: "/")
  let segments = list.take(segments, list.length(segments) - 1)
  string.join(segments, with: "/")
}

pub fn find_parent_gleam_toml_directory(path: String) {
  let error = "No gleam.toml parent directory"
  use <- bool.guard(when: string.is_empty(path), return: snag.error(error))
  let filename = string.join([path, "gleam.toml"], with: "/")
  fs.read_file(filename)
  |> result.replace(path)
  |> result.try_recover(fn(_) {
    let path = remove_last_segment(path)
    find_parent_gleam_toml_directory(path)
  })
}

pub fn at(list: List(a), index: Int) {
  use <- bool.guard(when: index < 0, return: Error(Nil))
  case list {
    [] -> Error(Nil)
    [elem, ..] if index == 0 -> Ok(elem)
    [_, ..rest] -> at(rest, index - 1)
  }
}
