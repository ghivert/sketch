import glance as g
import gleam/result
import gleam/string
import sketch/css/fs
import sketch/css/module/pipes
import snag

/// Definition of a Gleam styles definitions file. `path` is up to you, to
/// retrieve your module easily. `content` should be a valid Gleam source file.
pub type Module {
  Module(path: String, content: String, ast: g.Module)
}

/// Convert a file path into a parsed Gleam module. Every invalid Gleam module
/// will be rejected.
pub fn from_path(path: String) -> snag.Result(Module) {
  use content <- result.try(fs.read_file(path))
  use ast <- result.map(parse_module(content))
  Module(path:, content:, ast:)
}

pub fn remove_pipes(module: Module) {
  let ast = pipes.remove(module.ast)
  Module(..module, ast:)
}

fn parse_module(source: String) -> snag.Result(g.Module) {
  g.module(source)
  |> snag.map_error(string.inspect)
  |> snag.context("Illegal Gleam file")
}
