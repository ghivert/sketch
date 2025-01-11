import gleam/bool
import gleam/io
import gleam/list
import gleam/pair
import gleam/result
import gleam/string
import sketch
import sketch_css/fs
import sketch_css/module.{type Module}
import sketch_css/module/stylesheet.{type StyleSheet}
import sketch_css/utils.{type Directories}
import snag

/// Generate stylesheets from Gleam style definitions files. Recursively extract
/// all files ending with `_styles.gleam`, `_css.gleam` or `_sketch.gleam` to
/// proper stylesheets, and output some files interfaces to interact with them.
pub fn stylesheets(
  directories directories: Directories,
) -> Result(Nil, snag.Snag) {
  use is_dir <- result.try(fs.is_directory(directories.src))
  use <- bool.guard(when: !is_dir, return: snag.error("Not a directory"))
  use source_files <- result.try(fs.readdir(directories.src, recursive: True))
  use modules <- result.try(convert(source_files, directories))
  write_css_files(modules, directories)
}

fn convert(source_files: List(String), directories: Directories) {
  source_files
  |> list.filter_map(module.from_path(_, directories.src))
  |> list.map(module.remove_pipes)
  |> list.map(module.rewrite_imports)
  |> list.map(module.rewrite_exposings)
  |> module.reject_cycles
  |> result.map(convert_modules)
}

fn convert_modules(modules: List(Module)) -> List(#(Module, StyleSheet)) {
  list.fold(modules, [], module.convert_style(modules))
  |> list.filter_map(fn(module) {
    list.find(modules, fn(m) { m.name == module.0 })
    |> result.map(pair.new(_, module.1))
  })
}

fn write_css_files(
  modules: List(#(Module, StyleSheet)),
  directories: Directories,
) -> snag.Result(Nil) {
  let assert Ok(stylesheet) = sketch.stylesheet(strategy: sketch.Ephemeral)
  let modules = list.filter(modules, is_css_file)
  let write_css_file = write_css_file(_, stylesheet, directories)
  list.map(modules, write_css_file)
  |> result.all
  |> result.replace(Nil)
}

fn write_css_file(
  module: #(Module, StyleSheet),
  stylesheet: sketch.StyleSheet,
  directories: Directories,
) -> snag.Result(Nil) {
  let #(stylesheet, names) = module.build_stylesheet(module, stylesheet)
  let content = sketch.render(stylesheet)
  let #(content, interfaces) =
    module.build_interface({ module.0 }.name, content, names)
  let outputs = utils.outputs(directories, { module.0 }.name)
  use _ <- result.try(fs.mkdir(outputs.dst, recursive: True))
  use _ <- result.try(fs.mkdir(outputs.interface, recursive: True))
  use _ <- result.try(fs.write_file(outputs.dst_file, content))
  use _ <- result.map(fs.write_file(outputs.interface_file, interfaces))
  io.println("=========")
  io.println("Gleam styles " <> { module.0 }.name <> " converted.")
  io.println("CSS file " <> outputs.dst_file <> " generated.")
  io.println("Interface file " <> outputs.interface_file <> " generated.")
  Nil
}

fn is_css_file(module: #(Module, a)) -> Bool {
  let path = { module.0 }.path
  string.ends_with(path, "_styles.gleam")
  || string.ends_with(path, "_css.gleam")
  || string.ends_with(path, "_sketch.gleam")
}
