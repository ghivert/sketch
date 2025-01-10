import glance as g
import gleam/list
import gleam/order
import gleam/pair
import gleam/result
import gleam/string
import sketch
import sketch_css/fs
import sketch_css/module/dependencies
import sketch_css/module/exposings
import sketch_css/module/imports
import sketch_css/module/pipes
import sketch_css/module/stylesheet
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
pub fn from_path(path: String, root: String) -> snag.Result(Module) {
  use content <- result.try(fs.read_file(path))
  use ast <- result.map(parse_module(content))
  Module(path:, content:, ast:, name: {
    path
    |> string.replace(each: root <> "/", with: "")
    |> string.replace(each: root, with: "")
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

/// Identify cycles and continue if no cycles are found.
pub fn reject_cycles(modules: List(Module)) -> snag.Result(List(Module)) {
  let modules_ = list.map(modules, to_assoc)
  list.try_map(modules_, dependencies.reject_cycles(_, [], modules_))
  |> result.replace(modules)
}

/// Sort modules by order of dependencies. If a module depends on another, it
/// will be placed later in the list, guaranteeing the dependency will be
/// compiled earlier.
pub fn by_dependent(modules: List(Module)) {
  fn(a: Module, b: Module) {
    let a = to_assoc(a)
    let b = to_assoc(b)
    let modules = list.map(modules, to_assoc)
    case dependencies.is_dependency(a, of: b, modules:) {
      True -> order.Lt
      False -> order.Gt
    }
  }
}

/// Convert every style module to CSS stylesheet.
pub fn convert_styles(
  modules: List(Module),
) -> List(#(Module, stylesheet.StyleSheet)) {
  list.map(modules, to_assoc)
  |> list.fold([], stylesheet.convert)
  |> list.filter_map(fn(module) {
    list.find(modules, fn(mod) { mod.name == module.0 })
    |> result.map(pair.new(_, module.1))
  })
}

/// Build Gleam interface from an association list mapping function name to
/// generated class name by Sketch.
pub fn build_interface(
  module_name: String,
  content: String,
  names: List(#(String, String)),
) {
  list.fold(names, #(content, []), fn(acc, val) {
    let #(content, interface) = acc
    let #(fun_name, gen_name) = val
    let module_name = string.replace(module_name, each: "/", with: "-")
    let class_name = string.join([module_name, fun_name], with: "_")
    content
    |> string.replace(each: gen_name, with: class_name)
    |> pair.new({
      let class_name = string.join(["\"", class_name, "\""], with: "")
      ["pub", "const", fun_name, "=", class_name]
      |> string.join(with: " ")
      |> list.prepend(interface, _)
    })
  })
  |> pair.map_second(string.join(_, with: "\n\n"))
}

/// Build Sketch Stylesheet & association list interface.
pub fn build_stylesheet(
  module: #(Module, stylesheet.StyleSheet),
  stylesheet: sketch.StyleSheet,
) -> #(sketch.StyleSheet, List(#(String, String))) {
  use stylesheet, class <- list.fold({ module.1 }.classes, #(stylesheet, []))
  let #(stylesheet, names) = stylesheet
  let #(class_name, class) = class
  let #(stylesheet, generated_class) = sketch.class_name(class, stylesheet)
  #(stylesheet, list.key_set(names, class_name, generated_class))
}

fn parse_module(source: String) -> snag.Result(g.Module) {
  g.module(source)
  |> snag.map_error(string.inspect)
  |> snag.context("Illegal Gleam file")
}

fn to_assoc(module: Module) {
  #(module.name, module.ast)
}