import glance as g
import gleam/result
import gleam/string
import sketch/css/fs
import sketch/css/module/exposings
import sketch/css/module/imports
import sketch/css/module/pipes
import sketch/css/utils
import snag

/// Definition of a Gleam styles definitions file.
/// - `path` points to a valid Gleam source file on disk.
/// - `content` is the file content pointed by `path`.
/// - `ast` contains the AST of the Gleam module.
pub type Module {
  Module(path: String, content: String, ast: g.Module, name: String)
}

/// Read the file located at `path`, and turns it into a `Module`. No
/// modification is applied. If the file does not exists or is not a valid
/// Gleam file, an error is returned.
pub fn from_path(path: String) -> snag.Result(Module) {
  use content <- result.try(fs.read_file(path))
  use ast <- result.try(parse_module(content))
  let dir = utils.remove_last_segment(path)
  use dir <- result.map(utils.find_parent_gleam_toml_directory(dir))
  let src = string.join([dir, "src/"], with: "/")
  let test_ = string.join([dir, "test/"], with: "/")
  Module(path:, content:, ast:, name: {
    path
    |> string.replace(each: src, with: "")
    |> string.replace(each: test_, with: "")
    |> string.replace(each: ".gleam", with: "")
  })
}

/// Rewrites every pipe (`|>`) to the proper function call. Because pipe is an
/// operator that disappears at runtime, it's useless to keep it in the AST.
pub fn remove_pipes(module: Module) -> Module {
  let ast = pipes.remove(module.ast)
  Module(..module, ast:)
}

/// Rewrites every module call from partially qualified to fully qualified.
/// Every `module.function` will be rewrote to `fully/qualified/module.function`.
/// Handles plain modules and aliases.
pub fn rewrite_imports(module: Module) -> Module {
  let ast = imports.rewrite(module.ast)
  Module(..module, ast:)
}

/// Rewrites every exposed functions, constants and types to their fully
/// qualified equivalent. `function` will be rewrote to `fully/qualified/module.function`.
pub fn rewrite_exposings(module: Module) -> Module {
  let ast = exposings.rewrite(module.ast)
  Module(..module, ast:)
}

fn parse_module(source: String) -> snag.Result(g.Module) {
  g.module(source)
  |> snag.map_error(string.inspect)
  |> snag.context("Illegal Gleam file")
}
