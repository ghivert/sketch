import glance as g
import gleam/result
import gleam/string
import sketch/css/fs
import sketch/css/module/exposings
import sketch/css/module/imports
import sketch/css/module/pipes
import snag

/// Definition of a Gleam styles definitions file. `path` is up to you, to
/// retrieve your module easily. `content` should be a valid Gleam source file.
/// Definition of
pub type Module {
  Module(path: String, content: String, ast: g.Module)
}

/// Read the file located at `path`, and turns it into a `Module`. No
/// modification is applied. If the file does not exists or is not a valid
/// Gleam file, an error is returned.
pub fn from_path(path: String) -> snag.Result(Module) {
  use content <- result.try(fs.read_file(path))
  use ast <- result.map(parse_module(content))
  Module(path:, content:, ast:)
}

/// Rewrites every pipe (`|>`) to the proper function call. Because pipe is an
/// operator that disappears at runtime, it's useless to keep it in the AST.
pub fn remove_pipes(module: Module) -> Module {
  let ast = pipes.remove(module.ast)
  Module(..module, ast:)
}

/// Rewrites every module call from partially qualified to fully qualified.
/// Every `module.function` will be renamed to `fully/qualified/module.function`.
/// Handles plain modules and aliases.
pub fn rewrite_imports(module: Module) -> Module {
  let ast = imports.rewrite(module.ast)
  Module(..module, ast:)
}

pub fn rewrite_exposings(module: Module) -> Module {
  let ast = exposings.rewrite(module.ast)
  Module(..module, ast:)
}

fn parse_module(source: String) -> snag.Result(g.Module) {
  g.module(source)
  |> snag.map_error(string.inspect)
  |> snag.context("Illegal Gleam file")
}
