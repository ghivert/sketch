import gleam/bool
import gleam/list
import gleam/result
import gleam/string
import pprint
import simplifile
import sketch/css/fs
import sketch/css/module
import sketch/css/utils
import snag

/// Generate stylesheets from Gleam style definitions files. Recursively extract
/// all files ending with `_styles.gleam`, `_css.gleam` or `_sketch.gleam` to
/// proper stylesheets, and output some files interfaces to interact with them.
///
/// `src` should be a relative path containing the source files.
/// `dst` should be a relative path where to output CSS files.
/// `interface` should be a relative path where to output Gleam files.
pub fn stylesheets(
  directories directories: utils.Directories,
) -> Result(Nil, snag.Snag) {
  use is_dir <- result.try({
    simplifile.is_directory(directories.src)
    |> snag.map_error(string.inspect)
    |> snag.context("Simplifile error")
  })
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
  // let modules = compute_styles_modules(modules, src_interfaces)

  // let _ = fs.mkdir(directories.dst, recursive: True)
  // use #(module, css_module) <- list.each(css_modules)
  // let dst_path =
  //   module.path
  //   |> string.replace(each: directories.src, with: directories.dst)
  //   |> string.replace(each: ".gleam", with: ".css")
  // let parent_dst_path = path.dirname(dst_path)
  // let _ = fs.mkdir(parent_dst_path, recursive: True)
  // let _ = fs.write_file(dst_path, string.join(css_module.content, "\n\n"))
  // let src_styles_path =
  //   string.replace(module.path, each: directories.src, with: src_interfaces)
  // let parent_src_styles_path = path.dirname(src_styles_path)
  // let _ = fs.mkdir(parent_src_styles_path, recursive: True)
  // let _ =
  //   simplifile.write(src_styles_path, {
  //     list.map(css_module.classes, fn(c) {
  //       "pub const " <> c.0 <> " = \"" <> c.1 <> "\""
  //     })
  //     |> string.join("\n\n")
  //   })
}

fn is_css_file(path: String) -> Bool {
  string.ends_with(path, "_styles.gleam")
  || string.ends_with(path, "_css.gleam")
  || string.ends_with(path, "_sketch.gleam")
}
