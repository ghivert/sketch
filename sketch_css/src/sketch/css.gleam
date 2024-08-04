import glance.{
  type Expression, Call, Discarded, Expression, Field, FieldAccess, List, Module,
  Named, String, Variable,
}
import gleam/bool
import gleam/float
import gleam/function
import gleam/int
import gleam/io
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/pair
import gleam/result
import gleam/string
import simplifile
import sketch/css/error

pub type Module {
  Module(path: String, content: String, ast: Option(glance.Module))
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
    [Module(path: path, content: content, ast: None)]
  })
}

fn parse_modules(modules: List(Module)) {
  use module <- list.filter_map(modules)
  use ast <- result.map(glance.module(module.content) |> error.glance)
  Module(..module, ast: Some(ast))
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
        None -> i.definition.module
        Some(Discarded(_)) -> i.definition.module
        Some(Named(s)) -> s
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
    None -> Error(Nil)
    Some(ast) ->
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

fn skip_not_css_properties(name) {
  use <- bool.guard(when: name == "compose", return: Error(Nil))
  use <- bool.guard(when: name == "none", return: Error(Nil))
  Ok(name)
}

fn class_body(
  imports: List(String),
  properties: List(#(String, String)),
  body: List(Expression),
) -> List(String) {
  use property <- list.filter_map(body)
  case property {
    Call(Variable(name), param) -> {
      use name <- result.try(list.key_find(properties, name))
      use name <- result.try(skip_not_css_properties(name))
      let is_property = name == "property"
      let is_areas = name == "grid_template_areas"
      use <- bool.guard(when: is_property, return: property_to_string(param))
      use <- bool.guard(when: is_areas, return: template_areas_to_string(param))
      css_property(name, param)
    }
    Call(FieldAccess(Variable(module_name), name), param) -> {
      let is_sketch = list.contains(imports, module_name)
      use <- bool.guard(when: !is_sketch, return: Error(Nil))
      let should_skip = result.is_error(skip_not_css_properties(name))
      use <- bool.guard(when: should_skip, return: Error(Nil))
      let is_property = name == "property"
      let is_areas = name == "grid_template_areas"
      use <- bool.guard(when: is_property, return: property_to_string(param))
      use <- bool.guard(when: is_areas, return: template_areas_to_string(param))
      css_property(name, param)
    }
    _ -> Error(Nil)
  }
}

fn css_property(name, param) {
  let prop =
    string.split(name, "-")
    |> list.filter(fn(a) { a != "" })
    |> string.join("-")
  let body = property_body(param)
  Ok("  " <> prop <> ": " <> body <> ";")
}

fn property_to_string(param) {
  case param {
    [Field(None, String(param)), Field(None, String(content))] ->
      Ok("  " <> param <> ": " <> content <> ";")
    _ -> Error(Nil)
  }
}

fn template_areas_to_string(param) {
  case param {
    [Field(None, List(content, _))] ->
      Ok(
        "  grid-template-areas:\n"
        <> list.map(content, string_to_string)
        |> list.map(fn(a) { "    \"" <> a <> "\"" })
        |> string.join("\n")
        <> ";",
      )
    _ -> Error(Nil)
  }
}

fn string_to_string(param) {
  case param {
    glance.String(m) -> m
    _ -> ""
  }
}

fn property_body(param) {
  case param {
    [Field(None, String(content))] -> content
    [Field(None, Call(FieldAccess(Variable(_), size), [Field(None, value)]))] ->
      size_to_string(value) <> size
    [Field(None, Call(Variable(size), [Field(None, value)]))] ->
      size_to_string(value) <> size
    _ -> string.inspect(param)
  }
}

fn size_to_string(value) {
  case value {
    glance.Int(i) -> i
    glance.Float(f) -> f
    _ -> ""
  }
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
