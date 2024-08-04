import glance.{
  type Expression, Call, Discarded, Expression, Field, FieldAccess, List, Module,
  Named, Variable,
}
import gleam/bool
import gleam/function
import gleam/io
import gleam/list
import gleam/option
import gleam/pair
import gleam/result
import gleam/string
import simplifile
import sketch/css/error

pub type Module {
  Module(path: String, content: String, ast: option.Option(glance.Module))
}

pub type Css {
  Css(classes: List(#(String, String)), content: List(String))
}

/// Assumes dir is a directory. Should be checked before calling the function.
fn recursive_modules_read(dir: String) {
  use dir_content <- result.map(simplifile.read_directory(dir))
  list.flatten({
    use path <- list.filter_map(dir_content)
    let path = string.join([dir, path], "/")
    use is_dir <- result.try(simplifile.is_directory(path))
    use <- bool.guard(when: is_dir, return: recursive_modules_read(path))
    use content <- result.map(simplifile.read(path))
    [Module(path: path, content: content, ast: option.None)]
  })
}

fn parse_modules(modules: List(Module)) {
  use module <- list.filter_map(modules)
  use ast <- result.map(glance.module(module.content) |> error.glance)
  Module(..module, ast: option.Some(ast))
}

fn select_css_files(modules: List(Module)) {
  use module <- list.filter(modules)
  string.ends_with(module.path, "_styles.gleam")
  || string.ends_with(module.path, "_css.gleam")
  || string.ends_with(module.path, "_sketch.gleam")
}

fn find_sketch_imports(imports: List(glance.Definition(glance.Import))) {
  let imports = list.filter(imports, fn(i) { i.definition.module == "sketch" })
  let aliases =
    list.map(imports, fn(i) {
      case i.definition.alias {
        option.None -> i.definition.module
        option.Some(Discarded(_)) -> i.definition.module
        option.Some(Named(s)) -> s
      }
    })
    |> list.unique
  let exposed = {
    use i <- list.flat_map(imports)
    use value <- list.filter_map(i.definition.unqualified_values)
    use <- bool.guard(when: value.name != "class", return: Error(Nil))
    option.map(value.alias, Ok) |> option.unwrap(Ok(value.name))
  }
  let properties = {
    use i <- list.flat_map(imports)
    use value <- list.filter_map(i.definition.unqualified_values)
    use <- bool.guard(when: value.name == "class", return: Error(Nil))
    Ok(#(value.alias |> option.unwrap(value.name), value.name))
  }
  #(aliases, exposed, properties)
}

fn parse_css_modules(
  styles_modules: List(Module),
  _modules: List(Module),
) -> List(#(Module, Css)) {
  use styles_module <- list.filter_map(styles_modules)
  case styles_module.ast {
    option.None -> Error(Nil)
    option.Some(ast) ->
      Ok({
        #(styles_module, {
          let #(imports, exposed, properties) = find_sketch_imports(ast.imports)
          use css, function <- list.fold(ast.functions, Css([], []))
          function_definition(imports, exposed, properties, function.definition)
          |> result.map(fn(content) {
            let classes = list.prepend(css.classes, #(content.0, content.1))
            let content = list.prepend(css.content, content.2)
            Css(classes:, content:)
          })
          |> result.unwrap(css)
        })
      })
  }
}

fn keep_fn_call(function: glance.Function) {
  case function.body {
    [Expression(Call(call, [Field(_, List(body, _))]))] -> Ok(#(call, body))
    _ -> Error(Nil)
  }
}

fn keep_valid_class(
  imports: List(String),
  exposed: List(String),
  call: glance.Expression,
) {
  case call {
    Variable(fcall) -> {
      let is_class = !list.contains(exposed, fcall)
      use <- bool.guard(when: is_class, return: Error(Nil))
      Ok(Nil)
    }
    FieldAccess(Variable(module), fcall) -> {
      let not_class = fcall != "class"
      let not_sketch_class = !list.contains(imports, module) || not_class
      use <- bool.guard(when: not_sketch_class, return: Error(Nil))
      Ok(Nil)
    }
    _ -> Error(Nil)
  }
}

fn function_definition(
  imports: List(String),
  exposed: List(String),
  properties: List(#(String, String)),
  function: glance.Function,
) -> Result(#(String, String, String), Nil) {
  use #(call, body) <- result.try(keep_fn_call(function))
  use _ <- result.try(keep_valid_class(imports, exposed, call))
  let name = string.replace(function.name, each: "_", with: "-")
  let body = class_body(imports, properties, body) |> string.join("\n")
  let head = "." <> name <> " {\n"
  let body = head <> body <> "\n}"
  Ok(#(function.name, name, body))
}

fn skip(name) {
  use <- bool.guard(when: name == "compose", return: Error(Nil))
  Ok(name)
}

fn class_body(
  imports: List(String),
  properties: List(#(String, String)),
  body: List(Expression),
) -> List(String) {
  use property <- list.filter_map(body)
  case property {
    Call(Variable(name), param) ->
      list.key_find(properties, name)
      |> result.map(property_name)
      |> result.try(skip)
      |> result.map(fn(v) { "  " <> v <> ": " <> property_body(param) <> ";" })
    Call(FieldAccess(Variable(module_name), name), param) -> {
      let is_sketch = list.contains(imports, module_name)
      use <- bool.guard(when: !is_sketch, return: Error(Nil))
      use <- bool.guard(when: result.is_error(skip(name)), return: Error(Nil))
      let prop = property_name(name)
      Ok("  " <> prop <> ": " <> property_body(param) <> ";")
    }
    _ -> Error(Nil)
  }
}

fn property_name(name) {
  name
  |> string.split("-")
  |> list.filter(fn(a) { a != "" })
  |> string.join("-")
}

fn property_body(param) {
  string.inspect(param)
}

pub fn generate_stylesheets(src: String, dst: String) {
  use is_dir <- result.try(simplifile.is_directory(src) |> error.simplifile)
  use <- bool.guard(when: !is_dir, return: error.not_a_directory(src))
  use modules <- result.map(recursive_modules_read(src) |> error.simplifile)
  let modules = parse_modules(modules)
  let styles_modules = select_css_files(modules)
  let css_modules = parse_css_modules(styles_modules, modules)
  let _ = simplifile.create_directory_all(dst)
  use #(module, css_module) <- list.each(css_modules)
  let dst_path = string.replace(module.path, each: src, with: dst)
  let dst_path = string.replace(dst_path, each: ".gleam", with: ".css")
  let parent_dst_path =
    dst_path
    |> string.split("/")
    |> list.reverse
    |> list.drop(1)
    |> list.reverse()
    |> string.join("/")
  let _ = simplifile.create_directory_all(parent_dst_path)
  let _ = simplifile.write(dst_path, string.join(css_module.content, "\n\n"))
}
