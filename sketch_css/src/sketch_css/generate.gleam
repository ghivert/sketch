import gleam/bool
import gleam/list
import gleam/result
import gleam/string
import pprint
import sketch_css/fs
import sketch_css/module
import sketch_css/utils
import snag

/// Generate stylesheets from Gleam style definitions files. Recursively extract
/// all files ending with `_styles.gleam`, `_css.gleam` or `_sketch.gleam` to
/// proper stylesheets, and output some files interfaces to interact with them.
pub fn stylesheets(
  directories directories: utils.Directories,
) -> Result(Nil, snag.Snag) {
  use is_dir <- result.try(fs.is_directory(directories.src))
  use <- bool.guard(when: !is_dir, return: snag.error("Not a directory"))
  use source_files <- result.map(fs.readdir(directories.src, recursive: True))
  let _modules = convert(source_files)
  Nil
}

fn convert(source_files: List(String)) {
  source_files
  |> list.filter_map(module.from_path)
  |> list.map(module.remove_pipes)
  |> list.map(module.rewrite_imports)
  |> list.map(module.rewrite_exposings)
  |> module.reject_cycles
  |> result.map(fn(mods) { list.sort(mods, module.by_dependent(mods)) })
  |> result.map(module.convert_styles)
  |> result.map(list.filter(_, is_css_file))
  |> pprint.debug
}

fn is_css_file(module: #(module.Module, a)) -> Bool {
  let path = { module.0 }.path
  string.ends_with(path, "_styles.gleam")
  || string.ends_with(path, "_css.gleam")
  || string.ends_with(path, "_sketch.gleam")
}
