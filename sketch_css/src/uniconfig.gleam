import gleam/bool
import gleam/dict
import gleam/list
import gleam/pair
import gleam/result
import gleam/string
import sketch/css/fs
import snag
import tom

pub type Config {
  Config(directory: String, file: String, config: dict.Dict(String, tom.Toml))
}

pub fn read(package_name: String) -> snag.Result(Config) {
  use cwd <- result.try(fs.current_directory())
  do_read(cwd, package_name)
}

fn do_read(dir: String, package_name: String) -> snag.Result(Config) {
  use _ <- result.try_recover(package_config(dir, package_name))
  let error = "Impossible to find config"
  use <- bool.guard(when: string.is_empty(dir), return: snag.error(error))
  let segments = string.split(dir, on: "/")
  let segments = list.take(segments, list.length(segments) - 1)
  string.join(segments, with: "/")
  |> do_read(package_name)
}

fn package_config(dir: String, package_name: String) -> snag.Result(Config) {
  let filename = string.join([package_name, "toml"], with: ".")
  let filename = string.join([dir, filename], with: "/")
  let gleam_toml = string.join([dir, "gleam.toml"], with: "/")
  fs.read_file(filename)
  |> result.try(parse_toml)
  |> result.map(pair.new(filename, _))
  |> result.try_recover(fn(_) {
    fs.read_file(gleam_toml)
    |> result.try(parse_toml)
    |> result.then(fn(config) {
      tom.get_table(config, [package_name])
      |> snag.map_error(string.inspect)
      |> result.map(pair.new(gleam_toml, _))
    })
  })
  |> result.map(fn(config) {
    let #(file, config) = config
    let directory = add_leading_slash(dir)
    Config(config:, directory:, file:)
  })
}

fn parse_toml(content) {
  tom.parse(content)
  |> snag.map_error(string.inspect)
  |> snag.context("Unable to parse content")
  |> snag.context("content: " <> content)
}

fn add_leading_slash(dir: String) {
  use <- bool.guard(when: string.is_empty(dir), return: "/")
  dir
}
