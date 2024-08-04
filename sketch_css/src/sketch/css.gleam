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
  let #(all_classes, medias) = split_body(imports, properties, body)
  let body =
    list.map(all_classes, fn(class) {
      let #(selector, body) = class
      let body = class_body(imports, properties, body) |> string.join("\n")
      let head = "." <> name <> selector <> " {\n"
      head <> body <> "\n}"
    })
    |> string.join("\n\n")
  let body =
    body
    <> "\n\n"
    <> {
      list.map(medias, fn(media) {
        let #(media, body) = media
        let body =
          class_body(imports, properties, body)
          |> list.map(fn(s) { "  " <> s })
          |> string.join("\n")
        let head = "@media " <> media <> " {\n"
        let head = head <> "  ." <> name <> " {\n"
        head <> body <> "\n  }\n}"
      })
      |> string.join("\n\n")
    }
  Ok(#(function.name, name, body))
}

fn split_body(
  imports,
  properties,
  body,
) -> #(List(#(String, List(Expression))), List(#(String, List(Expression)))) {
  use results, item <- list.fold(body, #([], []))
  case item {
    Call(Variable(name), param) ->
      push_in_subclass(results, item, name, param, properties, True)
    Call(FieldAccess(Variable(module_name), name), param) -> {
      let is_sketch = list.contains(imports, module_name)
      use <- bool.guard(when: !is_sketch, return: Error(Nil))
      push_in_subclass(results, item, name, param, properties, False)
    }
    _ -> Error(Nil)
  }
  |> result.unwrap(results)
}

fn push_in_subclass(results, item, name, param, properties, unqualified) {
  let is_media = name == "media"
  let add_media = add_media(properties, results, param)
  use <- bool.lazy_guard(when: is_media, return: add_media)
  use name <- result.try(case unqualified {
    True -> list.key_find(properties, name)
    False -> Ok(name)
  })
  let is_pseudo = list.key_find(pseudo, name)
  let add_body = fn() { Ok(add_expressions(results, "", [item])) }
  use <- bool.lazy_guard(when: result.is_error(is_pseudo), return: add_body)
  let assert Ok(name) = is_pseudo
  case name, param {
    ":pseudo-selector",
      [Field(None, String(selector)), Field(None, List(expressions, _))]
    -> {
      Ok(add_expressions(results, selector, expressions))
    }
    ":nth-child",
      [Field(None, String(selector)), Field(None, List(expressions, _))]
    -> {
      Ok(add_expressions(results, name <> "(" <> selector <> ")", expressions))
    }
    ":nth-last-child",
      [Field(None, String(selector)), Field(None, List(expressions, _))]
    -> {
      Ok(add_expressions(results, name <> "(" <> selector <> ")", expressions))
    }
    ":nth-of-type",
      [Field(None, String(selector)), Field(None, List(expressions, _))]
    -> {
      Ok(add_expressions(results, name <> "(" <> selector <> ")", expressions))
    }
    ":nth-last-of-type",
      [Field(None, String(selector)), Field(None, List(expressions, _))]
    -> {
      Ok(add_expressions(results, name <> "(" <> selector <> ")", expressions))
    }
    _, [Field(None, List(expressions, _))] -> {
      Ok(add_expressions(results, name, expressions))
    }
    _, _ -> Error(Nil)
  }
}

fn add_media(
  properties,
  results: #(
    List(#(String, List(Expression))),
    List(#(String, List(Expression))),
  ),
  param,
) {
  fn() {
    let #(results, medias) = results
    case param {
      [Field(None, media), Field(None, List(content, _))] -> {
        use media <- result.try(media_to_string(properties, media))
        Ok(#(results, list.key_set(medias, media, content)))
      }
      _ -> Ok(#(results, medias))
    }
  }
}

fn media_to_string(properties, media) -> Result(String, Nil) {
  case rewrite_pipe(media) {
    Call(Variable(media), p) -> {
      use name <- result.try(list.key_find(properties, media))
      use <- bool.guard(when: name != "max_width", return: Error(Nil))
      Ok("(max-width: " <> property_body(p))
    }
    Call(FieldAccess(Variable(_), media), p) -> {
      case media {
        "max_width" | "min_width" | "max_height" | "min_height" -> {
          Ok(
            "("
            <> string.replace(media, "_", "-")
            <> ": "
            <> property_body(p)
            <> ")",
          )
        }
        "not" ->
          case p {
            [Field(None, m)] -> {
              use left <- result.try(media_to_string(properties, m))
              Ok("not(" <> left <> ")")
            }
            _ -> Error(Nil)
          }
        "and" | "or" ->
          case p {
            [Field(None, m), Field(None, n)] -> {
              use left <- result.try(media_to_string(properties, m))
              use right <- result.try(media_to_string(properties, n))
              Ok(left <> " " <> media <> " " <> right)
            }
            _ -> Error(Nil)
          }
        "landscape" | "portait" -> Ok("(orientation: " <> media <> ")")
        "dark_theme" | "light_theme" ->
          Ok(
            "(prefers-color-scheme: " <> string.replace(media, "_", "-") <> ")",
          )
        _ -> Error(Nil)
      }
    }
    _ -> Error(Nil)
  }
}

fn add_expressions(results, name, expressions) {
  let #(results, medias) = results
  #(
    list.key_find(results, name)
      |> result.unwrap([])
      |> list.append(expressions)
      |> list.key_set(results, name, _),
    medias,
  )
}

const pseudo = [
  #("placeholder", "::placeholder"), #("hover", ":hover"),
  #("active", ":active"), #("focus", ":focus"),
  #("focus_visible", ":focus_visible"), #("focus_within", ":focus-within"),
  #("enabled", ":enabled"), #("disabled", ":disabled"),
  #("read_only", ":read-only"), #("read_write", ":read-write"),
  #("checked", ":checked"), #("blank", ":blank"), #("valid", ":valid"),
  #("invalid", ":invalid"), #("required", ":required"),
  #("optional", ":optional"), #("link", ":link"), #("visited", ":visited"),
  #("target", ":target"), #("nth_child", ":nth-child"),
  #("nth_last_child", ":nth-last-child"), #("nth_of_type", ":nth-of-type"),
  #("nth_last_of_type", ":nth-last-of-type"), #("first_child", ":first-child"),
  #("last_child", ":last-child"), #("only_child", ":only-child"),
  #("first_of_type", ":first-of-type"), #("last_of_type", ":last-of-type"),
  #("only_of_type", ":only-of-type"), #("pseudo_selector", ":pseudo-selector"),
]

const skippable = ["compose", "none", "media"]

fn skip_not_css_properties(name) {
  case
    list.contains(skippable, name) || result.is_ok(list.key_find(pseudo, name))
  {
    True -> Error(Nil)
    False -> Ok(name)
  }
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
      compute_css_property(name, param)
    }
    Call(FieldAccess(Variable(module_name), name), param) -> {
      let is_sketch = list.contains(imports, module_name)
      use <- bool.guard(when: !is_sketch, return: Error(Nil))
      compute_css_property(name, param)
    }
    _ -> Error(Nil)
  }
}

fn compute_css_property(name, param) {
  use name <- result.try(skip_not_css_properties(name))
  let is_property = name == "property"
  let is_areas = name == "grid_template_areas"
  use <- bool.guard(when: is_property, return: property_to_string(param))
  use <- bool.guard(when: is_areas, return: template_areas_to_string(param))
  css_property(name, param)
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
    [Field(None, String(param)), Field(None, p)] ->
      Ok("  " <> param <> ": " <> string_to_string(p) <> ";")
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
    glance.Variable(v) ->
      "var(--" <> string.replace(v, each: "_", with: "-") <> ")"
    _ -> ""
  }
}

fn property_body(param) {
  case param {
    [Field(None, p)] -> {
      case rewrite_pipe(p) |> io.debug {
        Call(FieldAccess(Variable(_), size), [Field(None, value)]) ->
          size_to_string(value) <> size
        Call(Variable(size), [Field(None, value)]) ->
          size_to_string(value) <> size
        p -> string_to_string(p)
      }
    }
    _ -> string.inspect(param)
  }
}

fn size_to_string(value) {
  case rewrite_pipe(value) {
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

fn rewrite_pipe(expression) {
  case expression {
    glance.BinaryOperator(glance.Pipe, left, right) -> {
      case right {
        Call(f, args) -> Call(f, list.prepend(args, Field(None, left)))
        Variable(v) -> glance.Call(Variable(v), [Field(None, left)])
        _ -> expression
      }
    }
    _ -> expression
  }
}
