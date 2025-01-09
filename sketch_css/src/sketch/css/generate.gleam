import gleam/bool
import gleam/list
import gleam/result
import gleam/string
import pprint
import sketch/css/fs
import sketch/css/module
import sketch/css/utils
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
  let _modules =
    source_files
    |> list.filter(is_css_file)
    |> list.filter_map(module.from_path)
    |> list.map(module.remove_pipes)
    |> list.map(module.rewrite_imports)
    |> list.map(module.rewrite_exposings)
    |> pprint.debug
  Nil
}

fn is_css_file(path: String) -> Bool {
  string.ends_with(path, "_styles.gleam")
  || string.ends_with(path, "_css.gleam")
  || string.ends_with(path, "_sketch.gleam")
}
